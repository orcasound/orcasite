import { Socket } from 'phoenix'

if (!process.env.NEXT_PUBLIC_API_ENDPOINT) {
  throw new Error('NEXT_PUBLIC_API_ENDPOINT is not set')
}

const protocol = process.env.NODE_ENV === 'production' ? 'wss' : 'ws'
const socketUrl = `${protocol}://${process.env.NEXT_PUBLIC_API_ENDPOINT}/socket`

const isBrowser = typeof window !== 'undefined'
const socket = isBrowser ? new Socket(socketUrl) : null

socket?.connect()

export default socket
