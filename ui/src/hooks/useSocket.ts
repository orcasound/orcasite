import { Socket } from "phoenix";
import { useContext, useEffect } from "react";

import { useGetCurrentUserQuery } from "@/graphql/generated";
import { SocketContext } from "@/pages/_app";

const isBrowser = typeof window !== "undefined";

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
// Only called from inside an isBrowser guard.
function socketEndpoint(): string {
  if (allowInlinedConfig) {
    const override = process.env.NEXT_PUBLIC_SOCKET_ENDPOINT;
    if (override) return override;
  }

  const { protocol, host } = window.location;
  return `${protocol === "https:" ? "wss:" : "ws:"}//${host}/socket`;
}

export default function useSocket() {
  const { socket, setSocket } = useContext(SocketContext);

  const { currentUser } = useGetCurrentUserQuery().data ?? {};
  const userToken =
    typeof currentUser?.token === "string" ? currentUser?.token : undefined;

  useEffect(() => {
    let newSocket: Socket | undefined;
    if (isBrowser) {
      newSocket = new Socket(socketEndpoint(), {
        ...(userToken && { params: { token: userToken } }),
      });

      newSocket.connect();
      setSocket?.(newSocket);
    }
    return () => newSocket?.disconnect();
  }, [setSocket, userToken]);

  return socket?.current;
}
