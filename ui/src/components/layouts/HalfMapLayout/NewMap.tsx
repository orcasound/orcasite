import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { useRouter } from "next/router";
import { MutableRefObject, useEffect, useMemo, useRef, useState } from "react";
import {
  MapContainer,
  Marker,
  TileLayer,
  useMap,
  ZoomControl,
} from "react-leaflet";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import hydrophoneActiveIconImage from "@/public/icons/hydrophone-active.svg";
import hydrophoneDefaultIconImage from "@/public/icons/hydrophone-default.svg";
import { CombinedData } from "@/types/DataTypes";

function MapReadyLogger({
  mapRef,
}: {
  mapRef: MutableRefObject<LeafletMap | null>;
}) {
  const map = useMap();

  useEffect(() => {
    console.log("Map is ready via useMap:", map);
    if (mapRef) mapRef.current = map;
  }, [map]);

  return null;
}

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
  }, [feeds]);

  // TODO: where would it make sense to show all sightings in a longer time range?
  // const { filteredData } = useData();
  // const allSightings = filteredData.filter((el) => {
  //   return el.newCategory === "SIGHTINGS";
  // });

  // const [map, setMap] = useState<LeafletMap>();
  const mapRef = useRef<LeafletMap | null>(null);

  const [sightings, setSightings] = useState<CombinedData[]>();

  useEffect(() => {
    const sightingsNow = nowPlayingCandidate?.array?.filter((el) => {
      return el.newCategory === "SIGHTINGS";
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
      mapRef.current?.setZoom(12);
      mapRef.current?.panTo(feed.latLng);
    } else {
      mapRef.current?.setZoom(8);
    }
  }, [feed]);

  useEffect(() => {
    if (mapRef.current) {
      mapRef.current.invalidateSize();
    }
  }, [feeds]);

  return (
    <MapContainer
      center={[48.1, -122.75]} // this was formerly the center for the zoomed out but it overrides the first nowPlaying panTo in HalfMapLayout
      zoom={9}
      maxZoom={13}
      className="map-container"
      style={{ height: "100%", width: "100%" }}
      whenReady={() => {
        console.log("MapContainer ready");
        setTimeout(() => {
          mapRef.current?.invalidateSize(); // force reflow in case container was hidden on mount
        }, 500);
      }}
      // ref={(instance) => {
      //   if (instance) {
      //   mapRef.current = instance;
      // }
      // }}
      // whenReady={() => {
      //   console.log("Map is ready via whenReady")
      // }}
      zoomControl={false}
      //TODO: Disable attribution on mobile only
      attributionControl={false}
    >
      <MapReadyLogger mapRef={mapRef} />

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
        if (sighting.newCategory !== "SIGHTINGS") return null;
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
