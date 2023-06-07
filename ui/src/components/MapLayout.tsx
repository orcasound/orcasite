import { Box } from '@mui/material'
import type { Map as LeafletMap } from 'leaflet'
import dynamic from 'next/dynamic'
import { useRouter } from 'next/router'
import { ReactElement, ReactNode, useEffect, useState } from 'react'

import { GET_FEED, LIST_FEEDS } from '@/graphql/queries/feeds'
import { useGraphQL } from '@/hooks/useGraphQL'

import BottomNav from './BottomNav'
import Drawer from './Drawer'
import Header from './Header'
import Player from './Player'

const MapWithNoSSR = dynamic(() => import('./Map'), {
  ssr: false,
})

function MapLayout({ children }: { children: ReactNode }) {
  const router = useRouter()
  const slug = router.query.feed as string
  // TODO: don't make request if there's no feed slug
  const feed = useGraphQL(GET_FEED, { slug: slug }).data?.feed

  const [currentFeed, setCurrentFeed] = useState(feed)
  const [map, setMap] = useState<LeafletMap>()
  const feeds = useGraphQL(LIST_FEEDS).data?.feeds ?? []

  // update the currentFeed only if there's a new feed
  useEffect(() => {
    if (feed) {
      setCurrentFeed(feed)
      map?.panTo(feed.locationPoint.coordinates)
    }
  }, [feed, map])

  const invalidateSize = () => {
    if (map) {
      // wait 200ms before resizing so that drawer transition animations have a chance to finish
      // TODO: trigger resize directly from after transition instead of dead reckoning
      setTimeout(() => {
        map.invalidateSize({ pan: false })
      }, 200)
    }
  }

  return (
    <Box sx={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
      <Header />
      <Box sx={{ flexGrow: 1, display: 'flex' }}>
        <Drawer onClose={invalidateSize} onOpen={invalidateSize}>
          {children}
        </Drawer>
        <Box
          sx={{
            flexGrow: 1,
            display: 'flex',
            flexDirection: 'column',
          }}
        >
          <Box sx={{ flexGrow: 1 }}>
            <MapWithNoSSR
              setMap={setMap}
              currentFeed={currentFeed}
              feeds={feeds}
            />
          </Box>
          <Player currentFeed={currentFeed} />
        </Box>
      </Box>
      <BottomNav />
    </Box>
  )
}

export function getMapLayout(page: ReactElement) {
  return <MapLayout>{page}</MapLayout>
}
