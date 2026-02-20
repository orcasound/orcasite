import "leaflet/dist/leaflet.css";
import "leaflet-defaulticon-compatibility/dist/leaflet-defaulticon-compatibility.css";
import "leaflet-defaulticon-compatibility";

import { Map as LeafletMap } from "leaflet";
import L from "leaflet";
import { useRouter } from "next/router";
import { Fragment } from "react";
import {
  MapContainer,
  Marker,
  TileLayer,
  Tooltip,
  ZoomControl,
} from "react-leaflet";

import { Feed, FeedsQuery } from "@/graphql/generated";
import hydrophoneActiveIconImage from "@/public/icons/hydrophone-active.svg";
import hydrophoneDefaultIconImage from "@/public/icons/hydrophone-default.svg";
import { CascadiaSighting, DetectionsResult } from "@/types/DataTypes";
import formatDuration from "@/utils/dataHelpers";
// Added: new map helpers
import {
  AudibleRadiusCircles,
  LeafletTooltipGlobalStyles,
  ReportCount,
  sightingMarker,
} from "@/utils/mapHelpers";

export default function Map({
  setMap,
  currentFeed,
  feeds,
  sightings,
  detections,
}: {
  setMap?: (map: LeafletMap) => void;
  currentFeed?: Pick<Feed, "slug" | "latLng">;
  feeds: FeedsQuery["feeds"];
  sightings: CascadiaSighting[];
  detections: DetectionsResult[];
}) {
  const router = useRouter();

  const hydrophoneDefaultIcon = L.icon({
    iconUrl: hydrophoneDefaultIconImage.src,
    iconSize: [30, 30],
  });
  const hydrophoneActiveIcon = L.icon({
    iconUrl: hydrophoneActiveIconImage.src,
    iconSize: [30, 30],
  });

  return (
    <>
      <LeafletTooltipGlobalStyles />
      <MapContainer
        center={[48.27, -123.23]}
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

        {/* Feed icons with red circles for detection count and audible radius */}
        {feeds.map((feed) => {
          const audioDetectionsThisFeed = detections.filter(
            (d) => d.feedId === feed?.id,
          ).length;

          return (
            <Fragment key={feed.slug}>
              {feeds?.length && (
                <AudibleRadiusCircles centers={feeds.map((f) => f.latLng)} />
              )}

              <Marker
                key={feed.slug}
                position={feed.latLng}
                icon={
                  feed.slug === currentFeed?.slug
                    ? hydrophoneActiveIcon
                    : hydrophoneDefaultIcon
                }
                zIndexOffset={100}
              />

              <ReportCount
                center={feed.latLng}
                count={audioDetectionsThisFeed}
                onClick={() => {
                  router.push(`/listen/${feed.slug}`);
                }}
              />
            </Fragment>
          );
        })}

        {/* Blue sighting markers with tooltips */}
        {sightings?.map((sighting) => {
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
