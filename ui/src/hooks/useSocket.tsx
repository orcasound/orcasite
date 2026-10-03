import { Socket } from "phoenix";
import {
  createContext,
  ReactNode,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import { useGetCurrentUserQuery } from "@/graphql/generated";

// Duplicated rather than imported from utils/runtimeConfig -- see the note
// there. The minifier can only strip the NEXT_PUBLIC_* read below when this
// folds to false within this module; an imported binding is opaque to it.
const allowInlinedConfig =
  process.env.NODE_ENV === "development" ||
  process.env.NEXT_PUBLIC_ALLOW_INLINED_CONFIG === "true";

// Same-origin by default, for the same reason as the GraphQL endpoint: baking an
// absolute host into the bundle at build time makes the slug non-portable across
// Heroku apps. Phoenix serves both the page and /socket, so window.location is
// the right source. Local dev overrides via NEXT_PUBLIC_SOCKET_ENDPOINT, where
// Next and Phoenix sit on different ports -- gated on allowInlinedConfig so a
// stray config var on the build app cannot pin every environment to one host.
//
// Only called from an effect, so only in the browser.
function socketEndpoint(): string {
  if (allowInlinedConfig) {
    const override = process.env.NEXT_PUBLIC_SOCKET_ENDPOINT;
    if (override) return override;
  }

  const { protocol, host } = window.location;
  return `${protocol === "https:" ? "wss:" : "ws:"}//${host}/socket`;
}

// The same as the queries' staleTime in _app.tsx, so refreshing the token on a
// socket error fetches no more often than focus or navigation already can.
const TOKEN_REFRESH_INTERVAL_MS = 20 * 1000;

const SocketContext = createContext<{
  socket?: Socket;
  requestSocket?: () => void;
}>({});

/**
 * Owns the one Phoenix socket the app shares.
 *
 * The socket is opened the first time something calls `useSocket()`, so a
 * visit connects only once it reaches a page with live features, and then
 * stays connected. It is held in state, so replacing it re-renders every
 * consumer and their effects rejoin their channels on the new one.
 *
 * It is replaced only when the signed-in user changes. `currentUser.token` is
 * signed afresh on every read, so each refetch of the current user yields a new
 * string for the same user; rebuilding on that would drop every channel on each
 * refetch. The token is passed as a params function instead, which Phoenix
 * calls on each (re)connect, so a reconnect presents the latest one.
 *
 * The server rejects a token older than 24 hours. The token only changes when
 * the current user is refetched, which a long-lived tab that keeps focus may
 * never do, so a socket that fails to connect refetches it: otherwise Phoenix
 * would retry with the expired token indefinitely.
 */
export function SocketProvider({ children }: { children: ReactNode }) {
  const [requested, setRequested] = useState(false);
  const requestSocket = useCallback(() => setRequested(true), []);

  const { data, isPending, dataUpdatedAt, refetch } = useGetCurrentUserQuery(
    undefined,
    { enabled: requested },
  );
  const userId = data?.currentUser?.id;
  const token =
    typeof data?.currentUser?.token === "string"
      ? data.currentUser.token
      : undefined;

  const tokenRef = useRef(token);
  useEffect(() => {
    tokenRef.current = token;
  }, [token]);

  // Phoenix reports every failed attempt, every few seconds while the server is
  // unreachable, and a failed refetch leaves dataUpdatedAt where it was, so
  // throttle on the last attempt too.
  const refreshToken = useRef<() => void>(undefined);
  const lastRefresh = useRef(0);
  useEffect(() => {
    refreshToken.current = () => {
      const now = Date.now();
      const last = Math.max(dataUpdatedAt, lastRefresh.current);
      if (now - last < TOKEN_REFRESH_INTERVAL_MS) return;
      lastRefresh.current = now;
      refetch({ cancelRefetch: false });
    };
  }, [dataUpdatedAt, refetch]);

  const [socket, setSocket] = useState<Socket>();

  useEffect(() => {
    // Wait for the current user, so a signed-in visitor connects once with
    // their token rather than anonymously and then again. If the query fails
    // (e.g. /graphql erroring mid-deploy), isPending clears with no user, and a
    // signed-in visitor connects anonymously until the user is next refetched.
    // Live features still work; they just aren't attributed to the user.
    if (!requested || isPending) return;

    const newSocket = new Socket(socketEndpoint(), {
      params: () => (tokenRef.current ? { token: tokenRef.current } : {}),
    });
    newSocket.onError(() => refreshToken.current?.());
    newSocket.connect();
    setSocket(newSocket);

    return () => {
      newSocket.disconnect();
      // Unless a newer socket has already replaced it, don't leave consumers
      // holding one that is closed.
      setSocket((current) => (current === newSocket ? undefined : current));
    };
  }, [requested, isPending, userId]);

  const value = useMemo(
    () => ({ socket, requestSocket }),
    [socket, requestSocket],
  );

  return (
    <SocketContext.Provider value={value}>{children}</SocketContext.Provider>
  );
}

export default function useSocket() {
  const { socket, requestSocket } = useContext(SocketContext);

  useEffect(() => {
    requestSocket?.();
  }, [requestSocket]);

  return socket;
}
