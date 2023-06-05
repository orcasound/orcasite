import 'leaflet/dist/leaflet.css'
import 'leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css'
import 'leaflet-defaulticon-compatibility'

import { Map as LeafletMap } from 'leaflet'
import L from 'leaflet'
import { useRouter } from 'next/router'
import { MapContainer, Marker, TileLayer } from 'react-leaflet'

import hydrophoneActiveIconImage from '../../public/icons/hydrophone-active.svg'
import hydrophoneDefaultIconImage from '../../public/icons/hydrophone-default.svg'
import { Feed, FeedsQuery } from '../generated/types'

// Disable no-unused-modules because installed version of eslint-plugin-import
// can't handle dynamic imports yet
// TODO: Remove once eslint-plugin-import is upgraded
// eslint-disable-next-line import/no-unused-modules
export default function Map({
  setMap,
  currentFeed,
  feeds,
}: {
  setMap?: (map: LeafletMap) => void
  currentFeed?: Feed
  feeds: FeedsQuery['feeds']
}) {
  const router = useRouter()

  const hydrophoneDefaultIcon = L.icon({
    iconUrl: hydrophoneDefaultIconImage.src,
    iconSize: [30, 30],
  })
  const hydrophoneActiveIcon = L.icon({
    iconUrl: hydrophoneActiveIconImage.src,
    iconSize: [30, 30],
  })

  return (
    <MapContainer
      center={[48.27, -123.23]}
      zoom={9}
      maxZoom={13}
      style={{ height: '100%', width: '100%' }}
      ref={setMap}
      //TODO: Disable attribution on mobile only
      attributionControl={false}
    >
      <TileLayer
        attribution="Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"
        url="https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}"
      />
      <TileLayer url="https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/{z}/{y}/{x}" />

      {feeds.map((feed) => (
        <Marker
          key={feed.slug}
          position={feed.locationPoint.coordinates}
          icon={
            feed.slug === currentFeed?.slug
              ? hydrophoneActiveIcon
              : hydrophoneDefaultIcon
          }
          eventHandlers={{
            click: () => {
              router.push(`/${feed.slug}`)
            },
          }}
        />
      ))}
    </MapContainer>
  )
}
