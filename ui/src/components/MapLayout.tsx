import { Box } from '@mui/material'
import type { Map as LeafletMap } from 'leaflet'
import dynamic from 'next/dynamic'
import { ReactElement, ReactNode, useState } from 'react'

import BottomNav from './BottomNav'
import Drawer from './Drawer'
import Header from './Header'
import Player from './Player'

const MapWithNoSSR = dynamic(() => import('./Map'), {
  ssr: false,
})

function MapLayout({ children }: { children: ReactNode }) {
  const [map, setMap] = useState<LeafletMap>()

  const invalidateSize = () => {
    if (map) {
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
            <MapWithNoSSR setMap={setMap} />
          </Box>
          <Player />
        </Box>
      </Box>
      <BottomNav />
    </Box>
  )
}

export function getMapLayout(page: ReactElement) {
  return <MapLayout>{page}</MapLayout>
}
