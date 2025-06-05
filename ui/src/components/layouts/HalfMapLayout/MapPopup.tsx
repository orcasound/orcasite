import { Close } from "@mui/icons-material";
import {
  Box,
  Card,
  CardHeader,
  IconButton,
  Theme,
  Typography,
  useMediaQuery,
} from "@mui/material";

import { timeRangeSelect } from "@/components/CandidateList/CandidateListFilters";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
import { Sighting } from "@/types/DataTypes";
import formatDuration from "@/utils/masterDataHelpers";

function MapPopup({
  feed,
  sighting,
  onClick,
}: {
  feed: Feed | null;
  sighting: Sighting | null;
  onClick?: () => void;
}) {
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  // const { nowPlayingFeed, nowPlayingCandidate } = useNowPlaying();

  const {
    feeds,
    lastWhaleReport,
    // reportCount,
    filters,
  } = useData();

  if (sighting) {
    feed = feeds.find((f) => f.id === sighting?.feedId) ?? null;
  }

  const lastWhaleReportTime = lastWhaleReport(feed)?.timestampString;
  const lastWhaleReportSeconds = lastWhaleReportTime
    ? new Date(lastWhaleReportTime).getTime() / 1000
    : new Date().getTime() / 1000;
  const currentTimeSeconds = new Date().getTime() / 1000;
  const lastWhaleReportString = formatDuration(
    lastWhaleReportSeconds,
    currentTimeSeconds,
  );

  const href = `/beta/${feed?.slug}/candidates`;

  return (
    <Card
      className="popup-card"
      onClick={(e) => e.stopPropagation()}
      sx={{
        position: "absolute",
        bottom: smDown ? "1rem" : "2rem",
        right: smDown ? "1rem" : "2rem",
        zIndex: 10000,
        backgroundColor: "primary.main",
        boxShadow: "0px 2px 10px rgba(0,0,0,0.2)",
        width: smDown ? "calc(100% - 2rem)" : "calc(100% - 4rem)",
        color: "base.main",
      }}
    >
      <CardHeader
        avatar={
          feed &&
          sighting?.hydrophone !== "out of range" && (
            <Link
              href={href}
              // onClick={onClick}
            >
              <Box
                className="popup-thumbnail"
                sx={{
                  backgroundImage: `url(${feed?.imageUrl})`,
                  backgroundPosition: "center",
                  backgroundSize: "cover",
                  backgroundRepeat: "no-repeat",
                  width: smDown ? "40px" : "48px",
                  height: smDown ? "40px" : "48px",
                  borderRadius: "4px",
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                  padding: smDown ? "10px" : "1rem",
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
            // onClick={onClick}
          >
            <Typography
              variant="body1"
              component="h3"
              sx={{ fontWeight: "bold", color: "base.main", lineHeight: 1.3 }}
            >
              {sighting
                ? `${sighting?.hydrophone !== "out of range" ? `Sighting near` : "Sighting"} ${sighting?.hydrophone}`
                : feed?.name}
            </Typography>
          </Link>
        }
        subheader={
          <Typography
            variant={smDown ? "body2" : "body1"}
            sx={{ margin: 0, color: "base.main" }}
          >
            {lastWhaleReportString !== "audio unavailable" ? (
              <>
                {!sighting && "Last whale report – "}
                {lastWhaleReportString} ago
                {/* Lat:{" "}
            {feed ? feed.latLng.lat.toFixed(4) : sighting?.latitude?.toFixed(4)}
            {" · "}
            Lng:{" "}
            {feed
              ? feed.latLng.lng.toFixed(4)
              : sighting?.longitude?.toFixed(4)} */}
              </>
            ) : (
              <>
                {
                  timeRangeSelect.find((el) => el.value === filters.timeRange)
                    ?.label
                }{" "}
                – No whale reports
              </>
            )}
          </Typography>
        }
      />

      <Link
        href={href}
        style={{ textDecoration: "none" }}
        // onClick={onClick}
      >
        {/* <CardContent
          sx={{
            p: "10px",
            pt: 0,
            "&:last-child": {
              pb: "12px",
            },
          }}
        >
          <Typography variant="body2" sx={{ margin: 0, color: "base.main" }}>
            {sighting ? (
              <>
                {formatTimestamp(sighting.created)} — {sighting.name}
                <br />
                <span style={{ opacity: 0.75 }}>
                  {sighting.comments &&
                    cleanSightingsDescription(sighting.comments)}
                </span>
              </>
            ) : nowPlayingFeed ? (
              <>
                {
                  timeRangeSelect.find((el) => el.value === filters.timeRange)
                    ?.label
                }
                {" – "}
                {reportCount[feed ? feed.id : "all"].shortCountString}
              </>
            ) : nowPlayingCandidate ? (
              <>
                {formatTimestamp(nowPlayingCandidate.startTimestamp)} –{" "}
                {formatTimestamp(nowPlayingCandidate.endTimestamp)}
                <br />
                {reportCount[feed ? feed.id : "all"].countString}
              </>
            ) : (
              <></>
            )}
          </Typography>
        </CardContent> */}
      </Link>
    </Card>
  );
}

export default MapPopup;
