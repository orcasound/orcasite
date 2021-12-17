import { ReactElement, ReactNode } from 'react'

import Drawer from './Drawer'

function MapLayout({ children }: { children: ReactNode }) {
  return <Drawer>{children}</Drawer>
}

export function getMapLayout(page: ReactElement) {
  return <MapLayout>{page}</MapLayout>
}
