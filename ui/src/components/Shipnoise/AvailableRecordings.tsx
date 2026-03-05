import { Box, IconButton, Paper, Stack, Typography } from "@mui/material";
import React, { useEffect, useMemo, useState } from "react";

import ShipnoiseDetectionsPlayer from "@/components/Shipnoise/ShipnoiseDetectionsPlayer";

export type RecordingEntry = {
  id?: string;
  vessel?: string | null;
  mmsi?: string | null;
  location: string;
  date?: string;
  time?: string;
  timestamp?: string | null;
  cpaDistanceMeters?: number | null;
  noiseLevelDb?: number | null;
  hlsUrl?: string | null;
  startOffsetSec?: number | null;
  endOffsetSec?: number | null;
};

interface AvailableRecordingsProps {
  recordings?: RecordingEntry[];
}

declare global {
  interface Window {
    mcpopup?: { open: () => void };
    mc4wp?: { forms: { show: () => void } };
  }
}

const MAILCHIMP_SCRIPT =
  "https://chimpstatic.com/mcjs-connected/js/users/30e5b89b891e7b961c63e7d39/2318c630b0adc777855362be3.js";

const PREFERRED_LOCATIONS = [
  "Sunset Bay",
  "Bush Point",
  "Port Townsend",
  "Orcasound Lab",
];

const AvailableRecordings: React.FC<AvailableRecordingsProps> = ({
  recordings = [],
}) => {
  const safeRecordings = useMemo(() => {
    return recordings
      .filter((record) => {
        return (
          typeof record.hlsUrl === "string" &&
          record.hlsUrl.trim().length > 0 &&
          record.startOffsetSec != null &&
          record.endOffsetSec != null
        );
      })
      .map((record) => ({
        ...record,
        location: record.location || "Unknown location",
      }));
  }, [recordings]);

  const [expandedLocations, setExpandedLocations] = useState<Set<string>>(
    new Set(),
  );

  const openHydrophoneLocation = (label: string) => {
    const normalizedLabel = label?.trim();
    if (!normalizedLabel) return;
    const acceptedLabel = normalizedLabel
      .toLowerCase()
      .replace(/\s+/g, "-")
      .replace(/[^\w-]/g, "");
    const url = `https://live.orcasound.net/listen/${acceptedLabel}`;
    window.open(url, "_blank");
  };

  useEffect(() => {
    const existingScripts = document.querySelectorAll(
      'script[src*="chimpstatic"]',
    );
    existingScripts.forEach((s) => s.remove());

    const script = document.createElement("script");
    script.id = "mcjs";
    script.src = MAILCHIMP_SCRIPT + "?v=" + new Date().getTime();
    script.async = true;
    document.body.appendChild(script);
  }, []);

  const groupedLocations = useMemo(() => {
    const grouped: Record<string, RecordingEntry[]> = {};
    safeRecordings.forEach((record) => {
      if (!grouped[record.location]) grouped[record.location] = [];
      grouped[record.location].push(record);
    });

    const getTimestamp = (record: RecordingEntry) => {
      if (record.timestamp) {
        const ts = new Date(record.timestamp).getTime();
        if (!isNaN(ts)) return ts;
      }
      if (record.date) {
        const dateTimeStr = record.time
          ? `${record.date} ${record.time}`
          : record.date;
        const ts = new Date(dateTimeStr).getTime();
        if (!isNaN(ts)) return ts;
      }
      return 0;
    };

    const sortDesc = (items: RecordingEntry[]) =>
      [...items].sort((a, b) => getTimestamp(b) - getTimestamp(a));

    const preferred = PREFERRED_LOCATIONS.map((label) => ({
      label,
      recordings: sortDesc(grouped[label] ?? []),
    }));

    const others = Object.keys(grouped)
      .filter((label) => !PREFERRED_LOCATIONS.includes(label))
      .map((label) => ({ label, recordings: sortDesc(grouped[label]) }));

    return [...preferred, ...others].sort(
      (a, b) => b.recordings.length - a.recordings.length,
    );
  }, [safeRecordings]);

  const totalRecordings = safeRecordings.length;
  const vesselIdDisplay = totalRecordings > 0 ? safeRecordings[0].vessel : null;
  const recordingsLabel = totalRecordings
    ? `(${totalRecordings} recording${totalRecordings === 1 ? "" : "s"})`
    : "";

  const handleToggleLocation = (location: string, hasRecordings: boolean) => {
    if (!hasRecordings) return;
    setExpandedLocations((prev) => {
      const next = new Set(prev);
      if (next.has(location)) next.delete(location);
      else next.add(location);
      return next;
    });
  };

  if (totalRecordings === 0) return null;

  return (
    <Box sx={{ mt: 3, width: "100%" }}>
      <Box
        sx={{
          mx: "auto",
          width: "100%",
          maxWidth: "90rem",
          px: { xs: 2, md: 0 },
        }}
      >
        {/* Header Bar */}
        <Box
          sx={{
            display: "flex",
            flexWrap: { xs: "wrap", md: "nowrap" },
            alignItems: "center",
            bgcolor: "#2D3147",
            px: { xs: 2, md: "25px" },
            py: 3,
            height: { md: 64 },
          }}
        >
          <Box
            sx={{
              mr: 1,
              width: 20,
              height: 24,
              display: "flex",
              alignItems: "center",
            }}
          >
            {/* eslint-disable-next-line @next/next/no-img-element */}
            <img
              src="/shipnoise/VesselIcon.png"
              alt="Vessel"
              width={23}
              height={28}
              style={{ width: "100%", height: "100%" }}
            />
          </Box>
          <Typography
            variant="h6"
            sx={{
              color: "white",
              fontWeight: 600,
              fontSize: { xs: 18, md: 20 },
            }}
          >
            Explore Recordings of Vessel
            {vesselIdDisplay && (
              <>
                {` ${vesselIdDisplay}`}
                {recordingsLabel && (
                  <Box
                    component="span"
                    sx={{
                      fontSize: "22px",
                      fontWeight: 300,
                      lineHeight: "28px",
                      fontFamily: "Montserrat, sans-serif",
                    }}
                  >
                    {" "}
                    {recordingsLabel}
                  </Box>
                )}
              </>
            )}
          </Typography>
        </Box>

        {/* Location Accordions */}
        <Stack spacing={2}>
          {groupedLocations.map(({ label, recordings: groupedRecordings }) => {
            const isExpanded = expandedLocations.has(label);
            const hasRecordings = groupedRecordings.length > 0;
            const countLabel = groupedRecordings.length;

            return (
              <Paper
                key={label}
                square
                elevation={0}
                sx={{
                  width: "100%",
                  overflow: "hidden",
                  bgcolor: "#E5E7EB",
                  boxShadow: "none",
                }}
              >
                <Stack
                  direction={{ xs: "column", md: "row" }}
                  spacing={{ xs: 1, md: 2 }}
                  alignItems={{ md: "center" }}
                  justifyContent={{ md: "space-between" }}
                  sx={{
                    px: { xs: 2, md: "25px" },
                    py: 1.5,
                    height: { md: 46 },
                  }}
                >
                  <Typography
                    sx={{
                      textAlign: "left",
                      color: "#111827",
                      fontSize: "22px",
                      fontWeight: 600,
                      lineHeight: "28px",
                      fontFamily: "Montserrat, sans-serif",
                    }}
                  >
                    <Box
                      component="button"
                      onClick={() => openHydrophoneLocation(label)}
                      sx={{
                        cursor: "pointer",
                        border: "none",
                        background: "transparent",
                        padding: 0,
                        color: "inherit",
                        font: "inherit",
                        textDecoration: "none",
                        "&:hover": { textDecoration: "underline" },
                      }}
                    >
                      {label}
                    </Box>{" "}
                    <Box
                      component="span"
                      sx={{
                        fontSize: "22px",
                        fontWeight: 300,
                        lineHeight: "28px",
                        fontFamily: "Montserrat, sans-serif",
                      }}
                    >
                      ({countLabel} recording{countLabel === 1 ? "" : "s"})
                    </Box>
                  </Typography>
                  <Box sx={{ display: "flex", justifyContent: "flex-end" }}>
                    {hasRecordings ? (
                      <IconButton
                        onClick={() =>
                          handleToggleLocation(label, hasRecordings)
                        }
                        aria-expanded={isExpanded}
                        aria-label={
                          isExpanded ? `Collapse ${label}` : `Expand ${label}`
                        }
                        sx={{
                          width: 24,
                          height: 24,
                          p: 0,
                          transform: isExpanded
                            ? "rotate(0deg)"
                            : "rotate(180deg)",
                          transition: "transform 0.2s ease",
                        }}
                      >
                        {/* eslint-disable-next-line @next/next/no-img-element */}
                        <img
                          src="/shipnoise/up.svg"
                          alt=""
                          width={20}
                          height={20}
                          style={{ width: "100%", height: "100%" }}
                        />
                      </IconButton>
                    ) : (
                      <Box sx={{ width: 20, height: 20 }} aria-hidden />
                    )}
                  </Box>
                </Stack>

                {isExpanded && hasRecordings && (
                  <Box sx={{ bgcolor: "white" }}>
                    {groupedRecordings.map((rec, idx) => {
                      const uniqueKey = rec.id ?? `rec-${idx}`;

                      return (
                        <Box
                          key={uniqueKey}
                          sx={{
                            width: "100%",
                            borderBottom: "1px solid black",
                            px: { xs: 2, md: "45px" },
                            py: { xs: 2.5, md: "25px" },
                          }}
                        >
                          <ShipnoiseDetectionsPlayer
                            hlsUrl={rec.hlsUrl!}
                            startOffsetSec={rec.startOffsetSec!}
                            endOffsetSec={rec.endOffsetSec!}
                            timestamp={rec.timestamp}
                            date={rec.date}
                          />
                        </Box>
                      );
                    })}
                  </Box>
                )}
              </Paper>
            );
          })}
        </Stack>
      </Box>
    </Box>
  );
};

export default AvailableRecordings;
