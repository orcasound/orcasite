import { Box } from '@mui/material'
import type { Map as LeafletMap } from 'leaflet'
import dynamic from 'next/dynamic'
import { ReactElement, ReactNode, useEffect, useState } from 'react'

import { Feed, FeedsQuery } from '../generated/types'
import API from '../graphql/apiClient'
import BottomNav from './BottomNav'
import Drawer from './Drawer'
import Header from './Header'
import Player from './Player'

const MapWithNoSSR = dynamic(() => import('./Map'), {
  ssr: false,
})

type Feeds = FeedsQuery['feeds']

function MapLayout({
  children,
  feed,
  feeds,
}: {
  children: ReactNode
  feed?: Feed
  feeds: Feeds
}) {
  const [currentFeed, setCurrentFeed] = useState(feed)
  const [map, setMap] = useState<LeafletMap>()

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
  // if props include a feed, it gets passed to the layout so that the map and player can use the data
  const feed = page.props?.feed as Feed | undefined
  const feeds = page.props?.feeds as Feeds
  return (
    <MapLayout feed={feed} feeds={feeds}>
      {page}
    </MapLayout>
  )
}

export async function getMapProps() {
  const response = await API.feeds()
  return { feeds: response.feeds }
}
