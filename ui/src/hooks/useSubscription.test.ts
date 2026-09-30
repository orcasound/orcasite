import { renderHook } from "@testing-library/react";

import { useSubscription } from "./useSubscription";

// A Phoenix Push: callbacks registered with receive(status) fire on every reply
// with that status, which for the join push means every (re)join.
function fakePush() {
  const hooks: Record<string, ((response: unknown) => void)[]> = {};
  const push = {
    receive(status: string, callback: (response: unknown) => void) {
      (hooks[status] ??= []).push(callback);
      return push;
    },
    reply(status: string, response: unknown = {}) {
      hooks[status]?.forEach((callback) => callback(response));
    },
  };
  return push;
}

function fakeChannel(topic: string) {
  const joinPush = fakePush();
  const pushes: {
    event: string;
    payload: unknown;
    push: ReturnType<typeof fakePush>;
  }[] = [];
  return {
    topic,
    joinPush,
    pushes,
    handlers: {} as Record<string, (payload: unknown) => void>,
    join: vi.fn(() => joinPush),
    push: vi.fn((event: string, payload: unknown) => {
      const push = fakePush();
      pushes.push({ event, payload, push });
      return push;
    }),
    on(event: string, callback: (payload: unknown) => void) {
      this.handlers[event] = callback;
    },
    leave: vi.fn(),
  };
}

const socket = {
  channels: [] as ReturnType<typeof fakeChannel>[],
  channel(topic: string) {
    const channel = fakeChannel(topic);
    this.channels.push(channel);
    return channel;
  },
};

vi.mock("./useSocket", () => ({ default: () => socket }));

const query = { query: "subscription { thing }", variables: {} };

describe("useSubscription", () => {
  beforeEach(() => {
    socket.channels = [];
  });

  it("registers the document once the control channel has joined", () => {
    const onData = vi.fn();
    renderHook(() => useSubscription({ query, onData }));

    const [control] = socket.channels;
    expect(control.topic).toBe("__absinthe__:control");
    expect(control.pushes).toHaveLength(0);

    control.joinPush.reply("ok");
    expect(control.pushes.map((p) => p.event)).toEqual(["doc"]);

    control.pushes[0].push.reply("ok", {
      subscriptionId: "__absinthe__:doc:1",
    });
    const [, subscription] = socket.channels;
    expect(subscription.topic).toBe("__absinthe__:doc:1");

    subscription.handlers["subscription:data"]({
      result: { data: {} },
      subscriptionId: "1",
    });
    expect(onData).toHaveBeenCalledTimes(1);
  });

  it("registers the document again after a reconnect and tells the caller", () => {
    const onRejoin = vi.fn();
    renderHook(() => useSubscription({ query, onData: vi.fn(), onRejoin }));

    const [control] = socket.channels;
    control.joinPush.reply("ok");
    control.pushes[0].push.reply("ok", {
      subscriptionId: "__absinthe__:doc:1",
    });
    const [, firstSubscription] = socket.channels;
    expect(onRejoin).not.toHaveBeenCalled();

    // The socket dropped and came back: the control channel rejoins by itself
    control.joinPush.reply("ok");

    expect(control.pushes.map((p) => p.event)).toEqual(["doc", "doc"]);
    expect(firstSubscription.leave).toHaveBeenCalledTimes(1);
    expect(onRejoin).toHaveBeenCalledTimes(1);

    control.pushes[1].push.reply("ok", {
      subscriptionId: "__absinthe__:doc:2",
    });
    expect(socket.channels.map((c) => c.topic)).toEqual([
      "__absinthe__:control",
      "__absinthe__:doc:1",
      "__absinthe__:doc:2",
    ]);
  });

  it("leaves both channels on unmount", () => {
    const { unmount } = renderHook(() =>
      useSubscription({ query, onData: vi.fn() }),
    );
    const [control] = socket.channels;
    control.joinPush.reply("ok");
    control.pushes[0].push.reply("ok", {
      subscriptionId: "__absinthe__:doc:1",
    });

    unmount();

    expect(control.leave).toHaveBeenCalledTimes(1);
    expect(socket.channels[1].leave).toHaveBeenCalledTimes(1);
  });
});
