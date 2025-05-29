import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { GlobalStyles } from "@mui/material";
import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { LatLngExpression } from "leaflet";
import { useRouter } from "next/router";
import { Fragment, useEffect, useMemo, useState } from "react";
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

function AudibleRadiusCircles({ centers }: { centers: LatLngExpression[] }) {
  const map = useMap();

  useEffect(() => {
    const circles: L.Circle[] = [];

    centers.forEach((center) => {
      const circle = L.circle(center, {
        radius: 4828.03, // 3 miles in meters (1 mile = 1609.34 meters)
        color: "transparent",
        fillColor: "#ff0000",
        fillOpacity: 0.033,
      });
      circle.addTo(map);
      circles.push(circle);
    });

    return () => {
      circles.forEach((circle) => map.removeLayer(circle));
    };
  }, [centers, map]);

  return null;
}

function ReportCount({
  center,
  count,
}: {
  center: LatLngExpression;
  count: number;
}) {
  const map = useMap();

  useEffect(() => {
    if (!center) return;

    const countMarker = L.divIcon({
      html: `<div style="position: relative; z-index: 1001;">
         <span style="
           position: absolute;
           top: -10px;
           right: -10px;
           background: red;
           color: white;
           border-radius: 100%;
           padding: 2px 5px;
           font-size: 10px;
           min-width: 20px;
           min-height: 20px;
           display: flex;
           justify-content: center;
           align-items; center;
         ">${count}</span><div>`,
      className: "",
      iconSize: [30, 30],
      iconAnchor: [15, 15],
    });

    L.marker(center, { icon: countMarker, zIndexOffset: 1001 }).addTo(map);

    return () => {
      map.removeLayer(countMarker);
    };
  }, [center, count, map]);

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
          <Fragment key={f.slug}>
            {/* <AudibleRadius center={f.latLng} /> */}
            {feeds?.length && (
              <AudibleRadiusCircles centers={feeds.map((f) => f.latLng)} />
            )}

            <Marker
              position={f.latLng}
              icon={
                f.slug === feed?.slug
                  ? hydrophoneActiveIcon
                  : hydrophoneDefaultIcon
              }
              zIndexOffset={100}
              eventHandlers={{
                click: () => {
                  router.push(`/beta/${f.slug}/candidates`);
                },
              }}
            />
            <ReportCount
              center={f.latLng}
              count={filteredData.filter((d) => d.feedId === f?.id).length}
            />
          </Fragment>
        ))}
        {sightings?.map((sighting) => {
          if (sighting.newCategory !== "SIGHTING") return null;
          const inRange = sighting.hydrophone !== "out of range";
          return (
            <Marker
              key={sighting.id}
              zIndexOffset={0}
              position={[sighting.latitude, sighting.longitude]}
              opacity={inRange ? 1 : 0.33}
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
                  ${sighting.comments}<br />
                  Hydrophone: ${sighting.hydrophone}
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
