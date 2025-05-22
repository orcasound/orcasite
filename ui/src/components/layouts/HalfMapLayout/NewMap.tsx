import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { useRouter } from "next/router";
import { useEffect, useMemo, useState } from "react";
import { MapContainer, Marker, TileLayer, ZoomControl } from "react-leaflet";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import hydrophoneActiveIconImage from "@/public/icons/hydrophone-active.svg";
import hydrophoneDefaultIconImage from "@/public/icons/hydrophone-default.svg";
import { CombinedData } from "@/types/DataTypes";

export default function Map() {
  const router = useRouter();
  const { nowPlayingCandidate, nowPlayingFeed } = useNowPlaying();
  const { feeds } = useData();
  const feed = useMemo(() => {
    if (nowPlayingCandidate) {
      return feeds.find((f) => f.id === nowPlayingCandidate.feedId);
    } else {
      return nowPlayingFeed;
    }
  }, [nowPlayingCandidate, nowPlayingFeed, feeds]);

  // TODO: where would it make sense to show all sightings in a longer time range?
  // const { filteredData } = useData();
  // const allSightings = filteredData.filter((el) => {
  //   return el.newCategory === "SIGHTING";
  // });

  const [map, setMap] = useState<LeafletMap>();
  // const mapRef = useRef<LeafletMap | null>(null);

  const [sightings, setSightings] = useState<CombinedData[]>();
  useEffect(() => {
    const sightingsNow = nowPlayingCandidate?.array?.filter((el) => {
      return el.newCategory === "SIGHTING";
    });
    setSightings(sightingsNow);
  }, [nowPlayingCandidate]);

  const hydrophoneDefaultIcon = L.icon({
    iconUrl: hydrophoneDefaultIconImage.src,
    iconSize: [30, 30],
  });
  const hydrophoneActiveIcon = L.icon({
    iconUrl: hydrophoneActiveIconImage.src,
    iconSize: [30, 30],
  });

  useEffect(() => {
    if (feed) {
      map?.setZoom(12);
      map?.panTo(feed.latLng);
    } else {
      map?.setZoom(8);
    }
  }, [map, feed]);

  return (
    <MapContainer
      center={feed ? feed.latLng : [48.1, -122.75]} // this needs to be set or the map won't initialize
      zoom={9}
      maxZoom={13}
      className="map-container"
      style={{ height: "100%", width: "100%" }}
      ref={(instance) => {
        if (instance) {
          setMap(instance);
        }
      }}
      zoomControl={false}
      //TODO: Disable attribution on mobile only
      attributionControl={false}
    >
      <ZoomControl position="topright" />
      <TileLayer
        attribution="Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"
        url="https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}"
      />
      <TileLayer url="https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/{z}/{y}/{x}" />
      {feeds?.map((f) => (
        <Marker
          key={f.slug}
          position={f.latLng}
          icon={
            f.slug === feed?.slug ? hydrophoneActiveIcon : hydrophoneDefaultIcon
          }
          eventHandlers={{
            click: () => {
              router.push(`/listen/${feed?.slug}`);
            },
          }}
        />
      ))}
      {sightings?.map((sighting) => {
        if (sighting.newCategory !== "SIGHTING") return null;
        return (
          <Marker
            key={sighting.id}
            position={[sighting.latitude, sighting.longitude]}
          />
        );
      })}
    </MapContainer>
  );
}
