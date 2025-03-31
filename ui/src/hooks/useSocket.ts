import { Socket } from "phoenix";
import { useContext, useEffect } from "react";

import { useGetCurrentUserQuery } from "@/graphql/generated";
import { SocketContext } from "@/pages/_app";
if (!process.env.NEXT_PUBLIC_SOCKET_ENDPOINT) {
  throw new Error("NEXT_PUBLIC_SOCKET_ENDPOINT is not set");
}
const socketEndpoint = process.env.NEXT_PUBLIC_SOCKET_ENDPOINT;
const isBrowser = typeof window !== "undefined";

export default function useSocket() {
  const { socket, setSocket } = useContext(SocketContext);

  const { currentUser } = useGetCurrentUserQuery().data ?? {};
  const userToken =
    typeof currentUser?.token === "string" ? currentUser?.token : undefined;

  useEffect(() => {
    let socket: Socket | undefined;
    if (isBrowser) {
      const socket = new Socket(socketEndpoint, {
        ...(userToken && { params: { token: userToken } }),
      });

      socket.connect();
      setSocket?.(socket);
    }
    return () => socket?.disconnect();
  }, [setSocket, userToken]);

  return socket;
}
