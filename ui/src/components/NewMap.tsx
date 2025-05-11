import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { useRouter } from "next/router";
import { useEffect, useState } from "react";
import { MapContainer, Marker, TileLayer, ZoomControl } from "react-leaflet";

import { useNowPlaying } from "@/context/NowPlayingContext";
import { Feed, FeedsQuery } from "@/graphql/generated";
import hydrophoneActiveIconImage from "@/public/icons/hydrophone-active.svg";
import hydrophoneDefaultIconImage from "@/public/icons/hydrophone-default.svg";
import { CombinedData } from "@/types/DataTypes";

export default function Map({
  setMap,
  currentFeed,
  feeds,
}: {
  setMap?: (map: LeafletMap) => void;
  currentFeed?: Pick<Feed, "slug" | "latLng">;
  feeds?: FeedsQuery["feeds"];
}) {
  const router = useRouter();
  const { nowPlaying } = useNowPlaying();

  // TODO: where would it make sense to show all sightings in a longer time range?
  // const { filteredData } = useData();
  // const allSightings = filteredData.filter((el) => {
  //   return el.newCategory === "SIGHTINGS";
  // });

  const [sightings, setSightings] = useState<CombinedData[]>();
  useEffect(() => {
    const sightingsNow = nowPlaying?.array?.filter((el) => {
      return el.newCategory === "SIGHTINGS";
    });
    setSightings(sightingsNow);
  }, [nowPlaying]);

  const hydrophoneDefaultIcon = L.icon({
    iconUrl: hydrophoneDefaultIconImage.src,
    iconSize: [30, 30],
  });
  const hydrophoneActiveIcon = L.icon({
    iconUrl: hydrophoneActiveIconImage.src,
    iconSize: [30, 30],
  });

  return (
    <MapContainer
      // center={[48.1, -122.75]} // this was formerly the center for the zoomed out but it overrides the first nowPlaying panTo in HalfMapLayout
      zoom={9}
      maxZoom={13}
      style={{ height: "100%", width: "100%" }}
      ref={setMap}
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
      {feeds?.map((feed) => (
        <Marker
          key={feed.slug}
          position={feed.latLng}
          icon={
            feed.slug === currentFeed?.slug
              ? hydrophoneActiveIcon
              : hydrophoneDefaultIcon
          }
          eventHandlers={{
            click: () => {
              router.push(`/listen/${feed.slug}`);
            },
          }}
        />
      ))}
      {sightings?.map((sighting) => (
        <Marker
          key={sighting.id}
          position={[sighting.latitude, sighting.longitude]}
        />
      ))}
    </MapContainer>
  );
}
