import { Close } from "@mui/icons-material";
import { Box, Card, CardHeader, IconButton, Typography } from "@mui/material";

import { timeRangeSelect } from "@/components/CandidateList/CandidateListFilters";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
import { cleanSightingsDescription } from "@/hooks/useSortedCandidates";
import { Sighting } from "@/types/DataTypes";

function MapPopup({
  feed,
  sighting,
  onClick,
}: {
  feed: Feed | null;
  sighting: Sighting | null;
  onClick?: () => void;
}) {
  const { feeds, reportCount, filters } = useData();
  if (sighting) {
    feed = feeds.find((f) => f.id === sighting?.feedId) ?? null;
  }

  const href = `/beta/${feed?.slug}/candidates`;

  return (
    <Card
      className="popup-card"
      onClick={(e) => e.stopPropagation()}
      sx={{
        position: "absolute",
        bottom: "2rem",
        right: "2rem",
        zIndex: 10000,
        backgroundColor: "primary.main",
        boxShadow: "0px 2px 10px rgba(0,0,0,0.2)",
        width: "calc(100% - 4rem)",
        color: "base.main",
      }}
    >
      <CardHeader
        avatar={
          feed &&
          sighting?.hydrophone !== "out of range" && (
            <Link href={href} onClick={onClick}>
              <Box
                className="popup-thumbnail"
                sx={{
                  backgroundImage: `url(${feed?.imageUrl})`,
                  backgroundPosition: "center",
                  backgroundSize: "cover",
                  backgroundRepeat: "no-repeat",
                  width: "60px",
                  height: "60px",
                  borderRadius: "8px",
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                }}
              />
            </Link>
          )
        }
        action={
          <IconButton aria-label="close" onClick={onClick}>
            <Close sx={{ color: "base.main", fontSize: "1rem" }} />
          </IconButton>
        }
        title={
          <Link
            href={href}
            style={{ textDecoration: "none", color: "inherit" }}
            onClick={onClick}
          >
            <Typography
              variant="h6"
              component="h3"
              sx={{ fontWeight: "bold", color: "base.main" }}
            >
              {sighting
                ? `${sighting?.hydrophone !== "out of range" ? `Sighting near` : "Sighting"} ${sighting?.hydrophone}`
                : feed?.name}
            </Typography>
          </Link>
        }
        subheader={
          <Link
            href={href}
            style={{ textDecoration: "none" }}
            onClick={onClick}
          >
            <Typography sx={{ margin: 0, color: "base.main" }}>
              <span style={{ fontWeight: "400" }}>
                {sighting
                  ? `${sighting.name} · ${sighting.created}`
                  : `${timeRangeSelect.find((el) => el.value === filters.timeRange)?.label}: `}
              </span>
              <br />
              {sighting
                ? `${cleanSightingsDescription(sighting.comments)}`
                : `${reportCount[feed ? feed.id : "all"].countString}`}
              {/* Lat:{" "}
            {feed ? feed.latLng.lat.toFixed(4) : sighting?.latitude.toFixed(4)}
            {" • "}
            Lng:{" "}
            {feed ? feed.latLng.lng.toFixed(4) : sighting?.longitude.toFixed(4)} */}
            </Typography>
          </Link>
        }
      />
    </Card>
  );
}

export default MapPopup;
