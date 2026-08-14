import {
  Box,
  Button,
  IconButton,
  InputAdornment,
  List,
  ListItemButton,
  ListItemText,
  Paper,
  Stack,
  TextField,
  Typography,
} from "@mui/material";
import React, {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import AvailableRecordings, {
  type RecordingEntry,
} from "@/components/Shipnoise/AvailableRecordings";
import { useDebounce } from "@/hooks/useDebounce";
import {
  type ClipApiResult,
  type ClipsSearchParams,
  formatShipName,
  formatTitleCase,
  normalizeNameForSearch,
  SITE_LABELS,
  useClipsSearch,
  useVesselSearch,
  type VesselOption,
} from "@/hooks/useShipnoiseApi";

declare global {
  interface Window {
    gtag?: (...args: unknown[]) => void;
  }
}

type WarningInfo = {
  icon: string;
  content: React.ReactNode;
};

interface VesselInputProps {
  options: VesselOption[];
  onChange: (option: VesselOption | null) => void;
  placeholder: string;
  value?: string;
  onInputChange?: (value: string) => void;
}

const VesselInput: React.FC<VesselInputProps> = ({
  options,
  onChange,
  placeholder,
  value,
  onInputChange,
}) => {
  const [filteredOptions, setFilteredOptions] = useState<VesselOption[]>([]);
  const [showOptions, setShowOptions] = useState(false);
  const [suppressAutoOpen, setSuppressAutoOpen] = useState(true);
  const inputValue = value ?? "";
  const latestInputRef = useRef(inputValue);
  const containerRef = useRef<HTMLDivElement | null>(null);

  useEffect(() => {
    latestInputRef.current = inputValue;
  }, [inputValue]);

  const updateFilteredOptions = useCallback(
    (val: string, optionList: VesselOption[]) => {
      const normalizedVal = normalizeNameForSearch(val);
      if (!normalizedVal.length) {
        setFilteredOptions([]);
        setShowOptions(false);
        return;
      }
      const filtered = optionList.filter((opt) =>
        normalizeNameForSearch(opt.name).includes(normalizedVal),
      );
      setFilteredOptions(filtered);
      setShowOptions(!suppressAutoOpen && filtered.length > 0);
    },
    [suppressAutoOpen],
  );

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const val = e.target.value;
    setSuppressAutoOpen(false);
    onInputChange?.(val);
    updateFilteredOptions(val, options);
  };

  useEffect(() => {
    updateFilteredOptions(latestInputRef.current, options);
  }, [options, updateFilteredOptions]);

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        containerRef.current &&
        !containerRef.current.contains(event.target as Node)
      ) {
        setShowOptions(false);
      }
    };
    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  const handleInputFocus = () => {
    if (!suppressAutoOpen && filteredOptions.length > 0) setShowOptions(true);
  };

  const handleSelect = (option: VesselOption) => {
    setSuppressAutoOpen(true);
    setShowOptions(false);
    setFilteredOptions([]);
    onChange(option);
    onInputChange?.(option.name);
  };

  const handleClear = () => {
    setSuppressAutoOpen(false);
    setFilteredOptions([]);
    setShowOptions(false);
    onChange(null);
    onInputChange?.("");
  };

  return (
    <Box ref={containerRef} sx={{ position: "relative", width: "100%" }}>
      <TextField
        fullWidth
        value={inputValue}
        onChange={handleInputChange}
        onFocus={handleInputFocus}
        placeholder={placeholder}
        variant="outlined"
        size="small"
        autoComplete="off"
        InputProps={{
          endAdornment: inputValue ? (
            <InputAdornment position="end">
              <IconButton
                size="small"
                onClick={handleClear}
                aria-label="Clear"
                sx={{ p: 0.25 }}
              >
                {/* eslint-disable-next-line @next/next/no-img-element */}
                <img
                  src="/shipnoise/delete.svg"
                  alt="Clear"
                  width={20}
                  height={20}
                  style={{
                    width: "100%",
                    height: "100%",
                    objectFit: "contain",
                  }}
                />
              </IconButton>
            </InputAdornment>
          ) : null,
        }}
        sx={{
          "& .MuiOutlinedInput-root": {
            height: 42,
            borderRadius: "4px",
            backgroundColor: "white",
            "& fieldset": { borderColor: "#d1d5db" },
            "&:hover fieldset": { borderColor: "#111827" },
            "&.Mui-focused fieldset": { borderColor: "#111827" },
          },
          "& .MuiOutlinedInput-input": { px: { xs: 2, sm: 2.5 }, py: 1 },
        }}
      />
      {showOptions && filteredOptions.length > 0 && (
        <Paper
          elevation={6}
          sx={{
            position: "absolute",
            top: "100%",
            left: 0,
            right: 0,
            zIndex: 10,
            mt: 1,
            maxHeight: 192,
            overflowY: "auto",
            borderRadius: "4px",
            border: "1px solid #d1d5db",
          }}
        >
          <List dense disablePadding>
            {filteredOptions.map((opt, idx) => (
              <ListItemButton key={idx} onClick={() => handleSelect(opt)}>
                <ListItemText
                  primary={opt.name}
                  primaryTypographyProps={{ fontWeight: 500, color: "#1f2937" }}
                />
              </ListItemButton>
            ))}
          </List>
        </Paper>
      )}
    </Box>
  );
};

const normalizeClip = (clip: ClipApiResult): RecordingEntry => {
  const siteKey = clip.site?.replace(/\s+/g, "_").toLowerCase();
  const locationLabel =
    (siteKey && SITE_LABELS[siteKey]) ||
    (clip.site
      ? formatTitleCase(clip.site.replace(/[_\s]+/g, " "))
      : "Unknown site");

  const vesselName =
    formatShipName(clip.shipname) ??
    formatShipName(clip.mmsi ?? "") ??
    clip.shipname ??
    clip.mmsi ??
    "Unknown vessel";

  return {
    vessel: vesselName,
    mmsi: clip.mmsi ?? undefined,
    location: locationLabel,
    date: clip.date_utc,
    time: undefined,
    timestamp: clip.t_cpa ?? null,
    cpaDistanceMeters: clip.cpa_distance_m ?? undefined,
    noiseLevelDb: undefined,
    hlsUrl: clip.hls_url ?? null,
    startOffsetSec: clip.start_offset_sec ?? null,
    endOffsetSec: clip.end_offset_sec ?? null,
  };
};

const SelectionPanel = () => {
  const [selectedVessel, setSelectedVessel] = useState<VesselOption | null>(
    null,
  );
  const [vesselInputValue, setVesselInputValue] = useState("");
  const [warningInfo, setWarningInfo] = useState<WarningInfo | null>(null);
  const [hideDropdownSignal, setHideDropdownSignal] = useState(0);
  const [searchParams, setSearchParams] = useState<ClipsSearchParams | null>(
    null,
  );

  const debouncedInput = useDebounce(vesselInputValue, 300);
  const { data: vesselOptions = [] } = useVesselSearch(debouncedInput);
  const clipsQuery = useClipsSearch(searchParams);

  const recordings = useMemo<RecordingEntry[]>(() => {
    if (!clipsQuery.data) return [];
    const clips = Array.isArray(clipsQuery.data.results)
      ? clipsQuery.data.results
      : [];
    return clips.map(normalizeClip);
  }, [clipsQuery.data]);

  const dateRangeLabel = useMemo(() => {
    if (!clipsQuery.data) return undefined;
    const payload = clipsQuery.data;
    if (typeof payload.date_range_label === "string")
      return payload.date_range_label;
    if (payload.start_date && payload.end_date) {
      return payload.start_date === payload.end_date
        ? payload.start_date
        : `${payload.start_date} – ${payload.end_date}`;
    }
    return undefined;
  }, [clipsQuery.data]);

  const lastDataUpdatedAt = useRef(0);
  const selectedVesselRef = useRef(selectedVessel);
  selectedVesselRef.current = selectedVessel;
  const vesselInputRef = useRef(vesselInputValue);
  vesselInputRef.current = vesselInputValue;

  useEffect(() => {
    if (!clipsQuery.data || clipsQuery.isFetching) return;
    if (clipsQuery.dataUpdatedAt === lastDataUpdatedAt.current) return;
    lastDataUpdatedAt.current = clipsQuery.dataUpdatedAt;

    if (recordings.length === 0) {
      setWarningInfo({
        icon: "/shipnoise/Warning.svg",
        content: dateRangeLabel
          ? `No recordings match that vessel between ${dateRangeLabel}.`
          : "No recordings found for that vessel.",
      });
    } else {
      setWarningInfo(null);
      setHideDropdownSignal((prev) => prev + 1);

      const vesselLabel =
        (selectedVesselRef.current?.name ?? vesselInputRef.current.trim()) ||
        "ALL";
      window.gtag?.("event", "vessel_search", {
        event_category: "selection_panel",
        event_label: vesselLabel,
        vessel: vesselLabel,
        site: "ALL_SITES",
        date: new Date().toISOString().slice(0, 10),
        date_window: dateRangeLabel ?? "LAST_60_DAYS",
        date_range_label: dateRangeLabel ?? "LAST_60_DAYS",
      });
    }
  }, [
    clipsQuery.data,
    clipsQuery.isFetching,
    clipsQuery.dataUpdatedAt,
    recordings.length,
    dateRangeLabel,
  ]);

  useEffect(() => {
    if (!clipsQuery.error) return;
    const message = (clipsQuery.error as Error).message?.includes(
      "NEXT_PUBLIC_SHIPNOISE_API_URL",
    )
      ? "Search unavailable: backend URL is not configured."
      : "Unable to load recordings right now. Please try again.";
    setWarningInfo({ icon: "/shipnoise/Warning.svg", content: message });
  }, [clipsQuery.error]);

  const handleVesselInputChange = (value: string) => {
    setVesselInputValue(value);
    setWarningInfo(null);
    const normalizedValue = normalizeNameForSearch(value);
    const match = vesselOptions.find(
      (opt) => normalizeNameForSearch(opt.name) === normalizedValue,
    );
    setSelectedVessel(match ?? null);
  };

  const handleVesselSelect = (option: VesselOption | null) => {
    setSelectedVessel(option);
    setVesselInputValue(option?.name ?? "");
    setWarningInfo(null);
  };

  const handleSearchClick = () => {
    setWarningInfo(null);
    const searchDateIso = new Date().toISOString().slice(0, 10);
    const startDateObj = new Date(searchDateIso);
    startDateObj.setUTCDate(startDateObj.getUTCDate() - 59);
    setSearchParams({
      shipname: vesselInputValue.trim(),
      startDate: startDateObj.toISOString().slice(0, 10),
      endDate: searchDateIso,
    });
  };

  const showRecordings = recordings.length > 0 && !clipsQuery.isFetching;
  const isSearching = clipsQuery.isFetching;

  return (
    <Box sx={{ width: "100%", bgcolor: "white" }}>
      <Box
        sx={{
          width: "100%",
          px: { xs: 2, sm: 3 },
          pb: "30px",
          pt: { xs: 3, md: 5 },
        }}
      >
        <Stack
          sx={{ mx: "auto", width: "100%", maxWidth: "90rem" }}
          spacing={3.75}
        >
          {/* Search inputs */}
          <Box sx={{ display: "flex", justifyContent: "center" }}>
            <Paper
              variant="outlined"
              sx={{
                width: "100%",
                maxWidth: "90rem",
                borderRadius: "8px",
                borderWidth: 2,
                borderColor: "#e5e7eb",
                px: { xs: 2.5, md: "25px" },
                py: { xs: 3, md: "25px" },
                height: { md: 220 },
              }}
            >
              <Box sx={{ mb: { xs: 2, md: "15px" }, height: { md: 56 } }}>
                <Stack direction="row" alignItems="center" spacing={1}>
                  {/* eslint-disable-next-line @next/next/no-img-element */}
                  <img
                    src="/shipnoise/Search.svg"
                    alt="Search"
                    width={18}
                    height={18}
                    style={{ width: 18, height: 18, objectFit: "contain" }}
                  />
                  <Typography
                    sx={{
                      color: "#111827",
                      fontSize: "22px",
                      fontWeight: 600,
                      lineHeight: "28px",
                      fontFamily: "Montserrat, sans-serif",
                    }}
                  >
                    Explore Shipnoise Recordings
                  </Typography>
                </Stack>
                <Typography
                  sx={{
                    mt: 1,
                    color: "#9CA3AF",
                    fontSize: "18px",
                    lineHeight: "28px",
                    fontWeight: 500,
                  }}
                >
                  Enter a vessel name to discover and listen to its underwater
                  sound recordings
                </Typography>
              </Box>

              <Box sx={{ mt: 2.5, width: "100%" }}>
                <Stack
                  direction={{ xs: "column", md: "row" }}
                  spacing={{ xs: 2, md: "30px" }}
                  alignItems={{ xs: "stretch", md: "flex-end" }}
                  flexWrap={{ md: "wrap" }}
                >
                  <Box
                    sx={{
                      display: "flex",
                      flexDirection: "column",
                      gap: 1,
                      flex: 1,
                    }}
                  >
                    <Typography
                      sx={{
                        fontSize: "14px",
                        fontWeight: 500,
                        color: "#374151",
                        lineHeight: "20px",
                      }}
                    >
                      Vessel Name
                    </Typography>
                    <VesselInput
                      key={hideDropdownSignal}
                      options={vesselOptions}
                      onChange={handleVesselSelect}
                      onInputChange={handleVesselInputChange}
                      value={vesselInputValue}
                      placeholder="Enter vessel name"
                    />
                  </Box>

                  <Box
                    sx={{
                      width: { xs: "100%", md: 170 },
                      display: "flex",
                      flexDirection: "column",
                    }}
                  >
                    <Button
                      onClick={handleSearchClick}
                      disabled={!vesselInputValue.trim()}
                      variant="contained"
                      sx={{
                        height: 42,
                        width: "100%",
                        borderRadius: "100px",
                        textTransform: "none",
                        color: "white",
                        bgcolor: vesselInputValue.trim()
                          ? "black"
                          : "rgba(201,195,195,0.4)",
                        "&:hover": {
                          bgcolor: vesselInputValue.trim()
                            ? "rgba(0,0,0,0.9)"
                            : "rgba(201,195,195,0.4)",
                        },
                      }}
                    >
                      {isSearching ? "Loading…" : "Search"}
                    </Button>
                  </Box>
                </Stack>

                <Box sx={{ mt: 1, minHeight: 28 }}>
                  <Stack
                    direction="row"
                    spacing={1}
                    alignItems="center"
                    sx={{
                      maxWidth: 1031,
                      visibility: warningInfo ? "visible" : "hidden",
                    }}
                  >
                    <Box
                      sx={{
                        display: "flex",
                        width: 15,
                        height: 15,
                        flexShrink: 0,
                      }}
                    >
                      {warningInfo && (
                        // eslint-disable-next-line @next/next/no-img-element
                        <img
                          src={warningInfo.icon}
                          alt="Warning"
                          width={15}
                          height={15}
                          style={{
                            width: "100%",
                            height: "100%",
                            objectFit: "contain",
                          }}
                        />
                      )}
                    </Box>
                    <Typography
                      sx={{
                        fontSize: "14px",
                        lineHeight: "20px",
                        color: "#716E6E",
                        fontFamily: "Montserrat, sans-serif",
                      }}
                    >
                      {warningInfo?.content}
                    </Typography>
                  </Stack>
                </Box>
              </Box>
            </Paper>
          </Box>

          {showRecordings && <AvailableRecordings recordings={recordings} />}
        </Stack>
      </Box>
    </Box>
  );
};

export default SelectionPanel;
