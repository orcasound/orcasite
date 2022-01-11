import 'leaflet/dist/leaflet.css'

import { MapContainer, TileLayer } from 'react-leaflet'

// Disable no-unused-modules because installed version of eslint-plugin-import
// can't handle dynamic imports yet
// TODO: Remove once eslint-plugin-import is upgraded
// eslint-disable-next-line import/no-unused-modules
export default function Map() {
  return (
    <MapContainer
      center={[47.67, -122.59]}
      zoom={10}
      style={{ height: '100vh', width: '100%' }}
    >
      <TileLayer
        attribution='&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
      />
    </MapContainer>
  )
}
