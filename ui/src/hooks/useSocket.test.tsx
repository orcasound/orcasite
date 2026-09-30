import { renderHook } from "@testing-library/react";
import { ReactNode } from "react";

import useSocket, { SocketProvider } from "./useSocket";

class FakeSocket {
  params: () => Record<string, string>;
  errorCallbacks: (() => void)[] = [];
  connect = vi.fn();
  disconnect = vi.fn();
  constructor(
    public endPoint: string,
    opts: { params: () => Record<string, string> },
  ) {
    this.params = opts.params;
    sockets.push(this);
  }
  onError(callback: () => void) {
    this.errorCallbacks.push(callback);
  }
  // A failed connection attempt, as Phoenix reports it
  fail() {
    this.errorCallbacks.forEach((callback) => callback());
  }
}
let sockets: FakeSocket[] = [];

vi.mock("phoenix", () => ({
  Socket: vi.fn(function (
    endPoint: string,
    opts: { params: () => Record<string, string> },
  ) {
    return new FakeSocket(endPoint, opts);
  }),
}));

// What the current-user query returns; tests change it and rerender.
let currentUser: {
  isPending: boolean;
  dataUpdatedAt?: number;
  data?: { currentUser: { id: string; token: string } | null };
};
const refetch = vi.fn();
const queryOptions: { enabled?: boolean }[] = [];

vi.mock("@/graphql/generated", () => ({
  useGetCurrentUserQuery: (_: unknown, options: { enabled?: boolean }) => {
    queryOptions.push(options);
    return { dataUpdatedAt: Date.now(), ...currentUser, refetch };
  },
}));

const wrapper = ({ children }: { children: ReactNode }) => (
  <SocketProvider>{children}</SocketProvider>
);

const signedIn = (id: string, token: string) => ({
  isPending: false,
  data: { currentUser: { id, token } },
});

describe("SocketProvider", () => {
  beforeEach(() => {
    sockets = [];
    queryOptions.length = 0;
    currentUser = signedIn("user-1", "token-a");
  });

  it("does not ask for the current user or connect until something wants the socket", () => {
    renderHook(() => null, { wrapper });

    expect(queryOptions.every((o) => o.enabled === false)).toBe(true);
    expect(sockets).toHaveLength(0);
  });

  it("waits for the current user before connecting", () => {
    currentUser = { isPending: true };
    const { result, rerender } = renderHook(() => useSocket(), { wrapper });
    expect(sockets).toHaveLength(0);

    currentUser = signedIn("user-1", "token-a");
    rerender();

    expect(sockets).toHaveLength(1);
    expect(sockets[0].connect).toHaveBeenCalledTimes(1);
    expect(sockets[0].params()).toEqual({ token: "token-a" });
    expect(result.current).toBe(sockets[0]);
  });

  it("connects anonymously when nobody is signed in", () => {
    currentUser = { isPending: false, data: { currentUser: null } };
    renderHook(() => useSocket(), { wrapper });

    expect(sockets).toHaveLength(1);
    expect(sockets[0].params()).toEqual({});
  });

  it("keeps the socket when the same user's token is re-signed, and reconnects with the new one", () => {
    const { result, rerender } = renderHook(() => useSocket(), { wrapper });
    const [socket] = sockets;

    currentUser = signedIn("user-1", "token-b");
    rerender();

    expect(sockets).toHaveLength(1);
    expect(socket.disconnect).not.toHaveBeenCalled();
    expect(result.current).toBe(socket);
    // Phoenix calls params on each reconnect
    expect(socket.params()).toEqual({ token: "token-b" });
  });

  it("replaces the socket, and hands consumers the new one, when the user signs out", () => {
    const { result, rerender } = renderHook(() => useSocket(), { wrapper });
    const [first] = sockets;

    currentUser = { isPending: false, data: { currentUser: null } };
    rerender();

    expect(first.disconnect).toHaveBeenCalledTimes(1);
    expect(sockets).toHaveLength(2);
    expect(result.current).toBe(sockets[1]);
    expect(sockets[1].params()).toEqual({});
  });

  it("replaces the socket when a different user signs in", () => {
    const { result, rerender } = renderHook(() => useSocket(), { wrapper });
    const [first] = sockets;

    currentUser = signedIn("user-2", "token-c");
    rerender();

    expect(first.disconnect).toHaveBeenCalledTimes(1);
    expect(sockets).toHaveLength(2);
    expect(result.current).toBe(sockets[1]);
    expect(sockets[1].params()).toEqual({ token: "token-c" });
  });

  it("shares one socket between consumers", () => {
    const { result } = renderHook(() => [useSocket(), useSocket()], {
      wrapper,
    });

    expect(sockets).toHaveLength(1);
    expect(result.current[0]).toBe(result.current[1]);
  });

  it("leaves exactly one live socket under StrictMode", () => {
    const { result } = renderHook(() => useSocket(), {
      wrapper,
      reactStrictMode: true,
    });

    const live = sockets.filter((s) => !s.disconnect.mock.calls.length);
    expect(live).toHaveLength(1);
    expect(result.current).toBe(live[0]);
  });

  it("disconnects on unmount", () => {
    const { unmount } = renderHook(() => useSocket(), { wrapper });
    unmount();

    expect(sockets[0].disconnect).toHaveBeenCalledTimes(1);
  });

  describe("when the socket fails to connect", () => {
    beforeEach(() => {
      vi.useFakeTimers();
    });
    afterEach(() => {
      vi.useRealTimers();
    });

    it("refetches the current user, so the next attempt has a fresh token", () => {
      renderHook(() => useSocket(), { wrapper });

      // A token fetched just now is not the problem
      sockets[0].fail();
      expect(refetch).not.toHaveBeenCalled();

      vi.advanceTimersByTime(20_000);
      sockets[0].fail();
      expect(refetch).toHaveBeenCalledTimes(1);
      expect(refetch).toHaveBeenCalledWith({ cancelRefetch: false });
    });

    it("refetches at most once per 20 s while attempts keep failing", () => {
      renderHook(() => useSocket(), { wrapper });
      vi.advanceTimersByTime(20_000);

      // Phoenix retries every few seconds; the refetch fails, so the data's
      // timestamp never moves
      for (let i = 0; i < 8; i++) {
        sockets[0].fail();
        vi.advanceTimersByTime(5_000);
      }

      expect(refetch).toHaveBeenCalledTimes(2);
    });
  });
});
