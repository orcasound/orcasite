import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { GlobalStyles } from "@mui/material";
import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { LatLngExpression } from "leaflet";
import { useRouter } from "next/router";
import { useEffect, useMemo, useState } from "react";
import {
  MapContainer,
  Marker,
  TileLayer,
  Tooltip,
  ZoomControl,
} from "react-leaflet";
import { useMap } from "react-leaflet";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import hydrophoneActiveIconImage from "@/public/icons/hydrophone-active.svg";
import hydrophoneDefaultIconImage from "@/public/icons/hydrophone-default.svg";

function LeafletTooltipGlobalStyles() {
  return (
    <GlobalStyles
      styles={{
        ".leaflet-tooltip.custom-tooltip": {
          maxWidth: "300px",
          minWidth: "200px",
          textWrap: "wrap",
          // whiteSpace: "wrap",
          // wordWrap: "break-word",
          fontSize: "0.875rem", // Or use theme.typography.body2.fontSize if inside a function
          borderRadius: "4px",
          padding: "8px",
          boxShadow: "0px 1px 3px rgba(0, 0, 0, 0.2)",
        },
      }}
    />
  );
}

function MapUpdater({
  center,
  zoom,
}: {
  center: LatLngExpression;
  zoom: number;
}) {
  const map = useMap();

  useEffect(() => {
    if (center && zoom) {
      map.setView(center, zoom); // or map.panTo(center); map.setZoom(zoom);
    }
  }, [center, zoom, map]);

  return null;
}

export default function Map() {
  const router = useRouter();
  const { nowPlayingCandidate, nowPlayingFeed } = useNowPlaying();
  const { feeds, filteredData } = useData();

  const sightings = useMemo(() => {
    if (nowPlayingFeed) {
      return filteredData?.filter((d) => d.newCategory === "SIGHTING");
    } else if (nowPlayingCandidate) {
      const startDate = new Date(nowPlayingCandidate.startTimestamp);
      const endDate = new Date(nowPlayingCandidate.endTimestamp);
      return filteredData?.filter((d) => {
        return (
          d.newCategory === "SIGHTING" &&
          startDate <= new Date(d.timestampString) &&
          endDate >= new Date(d.timestampString)
        );
      });
    } else {
      return [];
    }
  }, [filteredData, nowPlayingCandidate, nowPlayingFeed]);

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
  const [latLng, setLatLng] = useState<LatLngExpression>([48.1, -122.75]);
  const [zoom, setZoom] = useState<number>(9);

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
      setLatLng(feed.latLng);
      setZoom(12);
      // map?.setZoom(12);
      // map?.panTo(feed.latLng);
    } else {
      setZoom(9);
      // map?.setZoom(8);
    }
  }, [map, feed, setLatLng, setZoom]);

  return (
    <>
      <LeafletTooltipGlobalStyles />
      <MapContainer
        center={feed ? feed.latLng : [48.1, -122.75]} // this needs to be set or the map won't initialize
        zoom={feed ? 12 : 9}
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
        <MapUpdater center={latLng} zoom={zoom} />
        {feeds?.map((f) => (
          <Marker
            key={f.slug}
            position={f.latLng}
            icon={
              f.slug === feed?.slug
                ? hydrophoneActiveIcon
                : hydrophoneDefaultIcon
            }
            eventHandlers={{
              click: () => {
                router.push(`/beta/${feed?.slug}`);
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
            >
              <Tooltip
                className="custom-tooltip"
                direction="top"
                offset={[0, 0]}
                opacity={1}
                permanent={false}
              >
                <div
                  dangerouslySetInnerHTML={{
                    __html: `
                  <strong>${sighting.name}</strong><br />
                  ${sighting.created}<br />
                  ${sighting.comments}
                  `,
                  }}
                />
              </Tooltip>
            </Marker>
          );
        })}
      </MapContainer>
    </>
  );
}
