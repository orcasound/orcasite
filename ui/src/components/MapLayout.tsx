import { ReactElement, ReactNode } from 'react'

function MapLayout({ children }: { children: ReactNode }) {
  return <div>{children}</div>
}

export function getMapLayout(page: ReactElement) {
  return <MapLayout>{page}</MapLayout>
}
