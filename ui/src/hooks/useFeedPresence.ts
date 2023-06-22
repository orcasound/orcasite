import { Channel, Presence } from 'phoenix'
import { useEffect, useRef, useState } from 'react'

import socket from '@/utils/socket'

/**
 * Hook to use Phoenix Presence for a specific feed
 * Automatically joins and leaves the feed channel
 * @returns Portion of Presence object pertaining to the feed
 */
export default function useFeedPresence(feedId?: string) {
  const channelRef = useRef<Channel>()
  const [feedPresence, setFeedPresence] = useState<Record<string, any>>()

  useEffect(() => {
    const channel = channelRef.current

    if (socket && feedId) {
      const newChannel = socket.channel(`feed:${feedId}`, {})
      channelRef.current = newChannel

      const presence = new Presence(newChannel)
      presence.onSync(() => {
        const state = Object.fromEntries(
          presence.list((id, pres) => [id, pres])
        )
        setFeedPresence(state[feedId])
      })
      newChannel.join()
    }

    return () => {
      channel?.leave()
    }
  }, [feedId])

  return feedPresence
}
