import { Socket } from "phoenix";

if (!process.env.NEXT_PUBLIC_SOCKET_ENDPOINT) {
  throw new Error("NEXT_PUBLIC_SOCKET_ENDPOINT is not set");
}

const socketEndpoint = process.env.NEXT_PUBLIC_SOCKET_ENDPOINT;

const isBrowser = typeof window !== "undefined";
const socket = isBrowser ? new Socket(socketEndpoint) : null;

socket?.connect();

export default socket;
