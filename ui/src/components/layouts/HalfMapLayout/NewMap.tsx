import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { GlobalStyles } from "@mui/material";
import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { LatLngExpression } from "leaflet";
import { useRouter } from "next/router";
import { Fragment, useEffect, useMemo, useState } from "react";
import { MapContainer, Marker, TileLayer, ZoomControl } from "react-leaflet";
import { useMap } from "react-leaflet";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Feed } from "@/graphql/generated";
import hydrophoneActiveIconImage from "@/public/icons/hydrophone-active.svg";
import hydrophoneDefaultIconImage from "@/public/icons/hydrophone-default.svg";
import { Sighting } from "@/types/DataTypes";

import MapPopup from "./MapPopup";

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

const materialLocationSvg = `<svg
      xmlns="http://www.w3.org/2000/svg"
      height="24px"
      viewBox="0 -960 960 960"
      width="24px"
      fill="#258dad"
    >
      <path d="M480-480q33 0 56.5-23.5T560-560q0-33-23.5-56.5T480-640q-33 0-56.5 23.5T400-560q0 33 23.5 56.5T480-480Zm0 400Q319-217 239.5-334.5T160-552q0-150 96.5-239T480-880q127 0 223.5 89T800-552q0 100-79.5 217.5T480-80Z" />
    </svg>`;

const sightingMarker = L.divIcon({
  html: materialLocationSvg,
  className: "",
  iconSize: [20, 20],
});

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
  onClick,
}: {
  center: LatLngExpression;
  count: number;
  onClick?: () => void;
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

    const marker = L.marker(center, {
      icon: countMarker,
      zIndexOffset: 1001,
    });

    if (onClick) {
      marker.on("click", onClick);
    }

    marker.addTo(map);

    return () => {
      marker.removeFrom(map);
    };
  }, [center, count, map, onClick]);

  return null;
}

function MapClickHandler({ onClick }: { onClick: () => void }) {
  const map = useMap();
  console.log("ran mapclickhandler");
  console.log("map instance", map);
  useEffect(() => {
    console.log("ran mapclickhandler useeffect");
    console.log("map instance useffect", map);
    map.on("click", onClick);
    return () => {
      map.off("click", onClick);
    };
  }, [map, onClick]);

  return null;
}

export default function Map() {
  const { nowPlayingCandidate, nowPlayingFeed } = useNowPlaying();
  const { feeds, filteredData } = useData();

  const [popupFeed, setPopupFeed] = useState<Feed | null>(null);
  const [popupDetection, setPopupDetection] = useState<Sighting | null>(null);

  // close the popup when the URL changes
  const router = useRouter();
  useEffect(() => {
    const handleRouteChange = () => {
      setPopupDetection(null);
      setPopupFeed(null);
    };
    router.events.on("routeChangeComplete", handleRouteChange);
    return () => {
      router.events.off("routeChangeComplete", handleRouteChange);
    };
  });

  const sightings = useMemo(() => {
    // show sightings within global filter time range in live player mode
    if (nowPlayingFeed) {
      return filteredData?.filter((d) => d.newCategory === "SIGHTING");
    } else if (nowPlayingCandidate) {
      // show sightings within candidate time range in candidate player mode
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
        <MapClickHandler
          onClick={() => {
            // close popup when clicking on the map
            setPopupFeed(null);
            setPopupDetection(null);
          }}
        />
        {feeds?.map((f) => (
          <Fragment key={f.slug}>
            {/* // necessary to map circles twice to make them all appear */}
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
            />
            <ReportCount
              center={f.latLng}
              count={
                filteredData.filter(
                  (d) => d.feedId === f?.id && d.newCategory !== "SIGHTING",
                ).length
              }
              onClick={() => {
                setPopupFeed(f);
                setPopupDetection(null);
              }}
            />
          </Fragment>
        ))}
        {sightings?.map((sighting) => {
          if (sighting.newCategory !== "SIGHTING") return null;
          const inRange = sighting.hydrophone !== "out of range";
          return (
            <Marker
              key={sighting.id}
              icon={sightingMarker}
              zIndexOffset={0}
              position={[sighting.latitude, sighting.longitude]}
              opacity={inRange ? 1 : 0.33}
              eventHandlers={{
                click: () => {
                  setPopupDetection(sighting);
                  setPopupFeed(null);
                },
              }}
            >
              {/* <Tooltip
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
              </Tooltip> */}
            </Marker>
          );
        })}
      </MapContainer>

      {(popupFeed || popupDetection) && (
        <MapPopup
          sighting={popupDetection}
          feed={popupFeed}
          onClick={() => {
            setPopupDetection(null);
            setPopupFeed(null);
          }}
        />
      )}
    </>
  );
}
