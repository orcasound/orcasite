import { GlobalStyles } from "@mui/material";
import L, { LatLngExpression } from "leaflet";
import { useEffect } from "react";
import { useMap } from "react-leaflet";

export function LeafletTooltipGlobalStyles() {
  return (
    <GlobalStyles
      styles={{
        ".leaflet-tooltip.custom-tooltip": {
          maxWidth: "300px",
          minWidth: "200px",
          textWrap: "wrap",
          fontSize: "0.875rem", // Or use theme.typography.body2.fontSize if inside a function
          borderRadius: "4px",
          padding: "8px",
          boxShadow: "0px 1px 3px rgba(0, 0, 0, 0.2)",
        },
      }}
    />
  );
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

export const sightingMarker = L.divIcon({
  html: materialLocationSvg,
  className: "",
  iconSize: [20, 20],
});

export function AudibleRadiusCircles({
  centers,
}: {
  centers: LatLngExpression[];
}) {
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

export function ReportCount({
  center,
  count,
  onClick,
}: {
  center: LatLngExpression;
  count?: number;
  onClick?: () => void;
}) {
  const map = useMap();

  if (!count) count = 0;

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
