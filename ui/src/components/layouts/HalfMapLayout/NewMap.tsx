import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { GlobalStyles, Theme, useMediaQuery } from "@mui/material";
import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { LatLngExpression } from "leaflet";
import { useRouter } from "next/router";
import { Fragment, useEffect, useMemo, useRef, useState } from "react";
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
import { Feed } from "@/graphql/generated";
import hydrophoneActiveIconImage from "@/public/icons/hydrophone-active.svg";
import hydrophoneDefaultIconImage from "@/public/icons/hydrophone-default.svg";
import { Sighting } from "@/types/DataTypes";
import formatDuration from "@/utils/masterDataHelpers";

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
  useEffect(() => {
    map.on("click", onClick);
    return () => {
      map.off("click", onClick);
    };
  }, [map, onClick]);

  return null;
}

export default function Map() {
  const {
    nowPlayingCandidate,
    nowPlayingFeed,
    setNowPlayingFeed,
    setNowPlayingCandidate,
  } = useNowPlaying();
  const { feeds, filteredData, autoPlayOnReady } = useData();

  const [popupFeed, setPopupFeed] = useState<Feed | null>(null);
  const [popupDetection, setPopupDetection] = useState<Sighting | null>(null);

  // close the popup when the feedSlug changes
  const router = useRouter();
  const previousFeedSlugRef = useRef<string | string[] | undefined>();

  useEffect(() => {
    const currentFeedSlug = router.query.feedSlug;

    // Run effect only if feedSlug changed
    if (previousFeedSlugRef.current !== currentFeedSlug) {
      previousFeedSlugRef.current = currentFeedSlug;

      setPopupDetection(null);
      setPopupFeed(null);
    }
  }, [router.query.feedSlug]);

  const reports = useMemo(() => {
    // show all reports in filter range for live player view
    if (nowPlayingFeed) {
      return filteredData;
    } else if (nowPlayingCandidate) {
      // show all reports in candidate range for live player view
      const startDate = new Date(nowPlayingCandidate.startTimestamp);
      const endDate = new Date(nowPlayingCandidate.endTimestamp);
      return filteredData?.filter((d) => {
        return (
          startDate <= new Date(d.timestampString) &&
          endDate >= new Date(d.timestampString)
        );
      });
    } else {
      return filteredData;
    }
  }, [filteredData, nowPlayingCandidate, nowPlayingFeed]);
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const audioReports = useMemo(() => {
    return reports?.filter((d) => d.newCategory !== "SIGHTING");
  }, [reports]);

  const sightings = useMemo(() => {
    return reports?.filter((d) => d.newCategory === "SIGHTING");
  }, [reports]);

  const feed = useMemo(() => {
    if (nowPlayingCandidate) {
      return feeds.find((f) => f.id === nowPlayingCandidate.feedId);
    } else {
      return nowPlayingFeed;
    }
  }, [nowPlayingCandidate, nowPlayingFeed, feeds]);

  const [map, setMap] = useState<LeafletMap>();
  const [latLng, setLatLng] = useState<LatLngExpression>([48.1, -122.75]);
  const [zoom, setZoom] = useState<number>(smDown ? 8 : 9);

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
      setZoom(smDown ? 11 : 12);
      // map?.setZoom(12);
      // map?.panTo(feed.latLng);
    } else {
      setZoom(smDown ? 8 : 9);
      // map?.setZoom(8);
    }
  }, [map, feed, setLatLng, setZoom, smDown]);

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
        {!smDown && <ZoomControl position="topright" />}
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
        {feeds?.map((f) => {
          const audioReportsThisFeed = audioReports.filter(
            (d) => d.feedId === f?.id,
          ).length;
          const sightingsThisFeed = sightings.filter(
            (s) => s.feedId === f?.id,
          ).length;
          return (
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
              {audioReportsThisFeed > 0 && (
                <ReportCount
                  center={f.latLng}
                  count={audioReportsThisFeed + sightingsThisFeed}
                  onClick={() => {
                    if (nowPlayingCandidate) {
                      router.push(`/beta/${f.slug}/candidates`);
                    } else if (f.id !== nowPlayingFeed?.id) {
                      autoPlayOnReady.current = false;
                      setNowPlayingFeed(f);
                      setNowPlayingCandidate(null);
                      // setPopupFeed(f);
                      // setPopupDetection(null);
                    } else {
                      router.push(`/beta/${f.slug}/candidates`);
                    }
                  }}
                />
              )}
            </Fragment>
          );
        })}
        {sightings?.map((sighting) => {
          if (sighting.newCategory !== "SIGHTING") return null;
          const inRange = sighting.hydrophone !== "out of range";
          const sightingTimeSeconds =
            new Date(sighting.created).getTime() / 1000;
          const currentTimeSeconds = new Date().getTime() / 1000;

          const timeAgo = formatDuration(
            sightingTimeSeconds,
            currentTimeSeconds,
          );

          return (
            <Marker
              key={sighting.id}
              icon={sightingMarker}
              zIndexOffset={0}
              position={[sighting.latitude, sighting.longitude]}
              opacity={inRange ? 1 : 0.33}
              eventHandlers={{
                click: () => {
                  if (smDown) {
                    setPopupDetection(sighting);
                    setPopupFeed(null);
                  }
                },
              }}
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
                  ${timeAgo} ago<br />
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
