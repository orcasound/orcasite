import dynamic from 'next/dynamic'
import { ReactElement, ReactNode } from 'react'

import Drawer from './Drawer'

const MapWithNoSSR = dynamic(() => import('./Map'), {
  ssr: false,
})

function MapLayout({ children }: { children: ReactNode }) {
  return (
    <>
      <Drawer>{children}</Drawer>
      <MapWithNoSSR />
    </>
  )
}

export function getMapLayout(page: ReactElement) {
  return <MapLayout>{page}</MapLayout>
}
