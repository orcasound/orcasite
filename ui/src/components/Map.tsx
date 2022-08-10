import 'leaflet/dist/leaflet.css'

import type { Map as LeafletMap } from 'leaflet'
import { MapContainer, TileLayer } from 'react-leaflet'

// Disable no-unused-modules because installed version of eslint-plugin-import
// can't handle dynamic imports yet
// TODO: Remove once eslint-plugin-import is upgraded
// eslint-disable-next-line import/no-unused-modules
export default function Map({
  setMap,
}: {
  setMap?: (map: LeafletMap) => void
}) {
  return (
    <MapContainer
      center={[48.27, -123.23]}
      zoom={9}
      style={{ height: '100%', width: '100%' }}
      ref={setMap}
      //TODO: Disable attribution on mobile only
      attributionControl={false}
    >
      <TileLayer
        attribution="Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"
        url="https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}"
      />
    </MapContainer>
  )
}
