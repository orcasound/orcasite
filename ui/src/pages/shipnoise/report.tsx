import {
  Box,
  Button,
  Container,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Typography,
} from "@mui/material";
import Head from "next/head";
import Link from "next/link";
import {
  ReactElement,
  type ReactNode,
  useEffect,
  useRef,
  useState,
} from "react";

import { getShipnoiseLayout } from "@/components/Shipnoise/ShipnoiseLayout";
import { type NextPageWithLayout } from "@/pages/_app";

interface Issue {
  [key: string]: string | number | boolean | null;
}

type DisplayField = {
  label: string;
  content: ReactNode;
};

const CHECKBOX_FIELDS = [
  {
    key: "Untitled checkboxes field (Bug/Software Malfunction)",
    label: "Bug / Software Malfunction",
  },
  {
    key: "Untitled checkboxes field (Data Inaccuracy)",
    label: "Data Inaccuracy",
  },
  {
    key: "Untitled checkboxes field (Performance Issue (Slow/Unresponsive))",
    label: "Performance Issue",
  },
  {
    key: "Untitled checkboxes field (User Interface/Experience Issue)",
    label: "UI / UX Issue",
  },
  {
    key: "Untitled checkboxes field (Security Vulnerability)",
    label: "Security Vulnerability",
  },
  {
    key: "Untitled checkboxes field (Feature Request/Suggestion)",
    label: "Feature Request",
  },
  {
    key: "Untitled checkboxes field (Other (Please describe in detail below))",
    label: "Other",
  },
];

const DETAIL_FIELDS = [
  { key: "Submission ID", label: "Submission ID" },
  { key: "Respondent ID", label: "Respondent ID" },
  { key: "Submitted at", label: "Submitted At" },
  {
    key: "What is the nature of the error you are reporting?",
    label: "Nature of Error",
  },
  {
    key: "Please describe the error in detail. What were you doing when the error occurred, and what was the unexpected behavior?",
    label: "Error Details",
  },
  {
    key: "What is the expected behavior when performing the actions that led to the error?",
    label: "Expected Behavior",
  },
  {
    key: "Have you found any workarounds for this error? If yes, please describe them.",
    label: "Workarounds",
  },
  {
    key: "If possible, please upload any relevant screenshots or error logs.",
    label: "Attachments",
  },
];

const DATE_FIELD_KEYS = [
  "Submitted at",
  "Submitted At",
  "Timestamp",
  "timestamp",
] as const;
const ATTACHMENT_FIELD_KEYS = new Set([
  "If possible, please upload any relevant screenshots or error logs.",
  "Attachments",
]);

function getIssueTimestamp(issue: Issue | null | undefined): number {
  if (!issue || typeof issue !== "object") return 0;
  for (const key of DATE_FIELD_KEYS) {
    const timestamp = parseTimestamp(issue[key]);
    if (timestamp !== null) return timestamp;
  }
  return 0;
}

function formatDateValue(raw: unknown): string {
  const timestamp = parseTimestamp(raw);
  if (timestamp === null) return "—";
  return formatUtcTimestamp(timestamp);
}

function parseTimestamp(raw: unknown): number | null {
  if (raw === null || raw === undefined) return null;
  if (typeof raw === "number") {
    if (!Number.isFinite(raw)) return null;
    return normalizeEpoch(raw);
  }
  if (typeof raw === "string") {
    const trimmed = raw.trim();
    if (!trimmed) return null;
    const direct = new Date(trimmed);
    if (!Number.isNaN(direct.getTime())) return direct.getTime();
    const asNumber = Number(trimmed);
    if (!Number.isNaN(asNumber)) return normalizeEpoch(asNumber);
  }
  return null;
}

function normalizeEpoch(value: number): number | null {
  if (!Number.isFinite(value)) return null;
  const asMillis = new Date(value);
  if (!Number.isNaN(asMillis.getTime()) && asMillis.getUTCFullYear() >= 2000) {
    return asMillis.getTime();
  }
  const asSeconds = new Date(value * 1000);
  if (!Number.isNaN(asSeconds.getTime())) return asSeconds.getTime();
  if (!Number.isNaN(asMillis.getTime())) return asMillis.getTime();
  return null;
}

function formatUtcTimestamp(epochMs: number): string {
  const date = new Date(epochMs);
  if (Number.isNaN(date.getTime())) return "—";
  const year = date.getUTCFullYear();
  const month = String(date.getUTCMonth() + 1).padStart(2, "0");
  const day = String(date.getUTCDate()).padStart(2, "0");
  const hours = String(date.getUTCHours()).padStart(2, "0");
  const minutes = String(date.getUTCMinutes()).padStart(2, "0");
  const seconds = String(date.getUTCSeconds()).padStart(2, "0");
  return `${year}-${month}-${day} ${hours}:${minutes}:${seconds} UTC+0`;
}

function toDisplayString(value: unknown): string {
  if (value === null || value === undefined) return "—";
  if (typeof value === "string") return value.trim() || "—";
  if (typeof value === "number" || typeof value === "boolean")
    return String(value);
  if (Array.isArray(value))
    return (
      value
        .map((v) => toDisplayString(v))
        .filter((v) => v !== "—")
        .join(", ") || "—"
    );
  return JSON.stringify(value);
}

function isTruthy(value: unknown): boolean {
  if (value === null || value === undefined) return false;
  if (typeof value === "boolean") return value;
  if (typeof value === "string") {
    const normalized = value.trim().toLowerCase();
    return normalized === "true" || normalized === "yes" || normalized === "1";
  }
  if (typeof value === "number") return value !== 0;
  return false;
}

function extractUrls(text: string): string[] {
  const regex = /https?:\/\/[^\s)]+/g;
  const matches = text.match(regex);
  if (!matches) return [];
  return matches.map((url) => url.replace(/[.,)]+$/, ""));
}

function collectAttachmentUrls(raw: unknown): string[] {
  if (!raw) return [];
  const fromArray = Array.isArray(raw)
    ? raw
        .map((item) =>
          typeof item === "string" ? item : toDisplayString(item),
        )
        .flatMap((item) => extractUrls(item))
    : extractUrls(typeof raw === "string" ? raw : toDisplayString(raw));
  return fromArray.filter(Boolean);
}

function renderAttachmentValue(raw: unknown): ReactNode {
  const urls = collectAttachmentUrls(raw);
  if (!urls.length) return <span>{toDisplayString(raw)}</span>;

  return urls.map((url, index) => (
    <Box
      key={url ?? index}
      sx={{ display: "flex", flexDirection: "column", gap: 0.5 }}
    >
      <Box
        component="a"
        href={url}
        target="_blank"
        rel="noreferrer"
        sx={{
          color: "#475569",
          textDecoration: "underline",
          textDecorationColor: "#cbd5e1",
          textUnderlineOffset: "4px",
          "&:hover": { color: "#1e293b" },
        }}
      >
        View attachment {urls.length > 1 ? index + 1 : ""}
      </Box>
      {/* eslint-disable-next-line @next/next/no-img-element */}
      <Box
        component="img"
        src={url}
        alt={`Attachment ${index + 1}`}
        loading="lazy"
        sx={{
          maxHeight: 320,
          width: "100%",
          borderRadius: "8px",
          border: "1px solid #e2e8f0",
          objectFit: "contain",
        }}
      />
    </Box>
  ));
}

function renderFieldValue(fieldKey: string, raw: unknown): ReactNode {
  if (
    DATE_FIELD_KEYS.map((k) => k.toLowerCase()).includes(fieldKey.toLowerCase())
  ) {
    return <span>{formatDateValue(raw)}</span>;
  }
  if (ATTACHMENT_FIELD_KEYS.has(fieldKey)) {
    return renderAttachmentValue(raw);
  }
  return <span>{toDisplayString(raw)}</span>;
}

function buildDisplayFields(issue: Issue): DisplayField[] {
  const entries: DisplayField[] = DETAIL_FIELDS.map(({ key, label }) => ({
    label,
    content: renderFieldValue(key, issue[key]),
  }));

  const checkboxSelections = CHECKBOX_FIELDS.filter(({ key }) =>
    isTruthy(issue[key]),
  ).map(({ label }) => label);

  entries.splice(4, 0, {
    label: "Error Categories",
    content: (
      <span>
        {checkboxSelections.length
          ? checkboxSelections.join(", ")
          : "None selected"}
      </span>
    ),
  });

  return entries;
}

const ShipnoiseReportPage: NextPageWithLayout = () => {
  const [issues, setIssues] = useState<Issue[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(false);
  const [selected, setSelected] = useState<Issue | null>(null);
  const dialogRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!selected) return;
    dialogRef.current?.focus();
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") setSelected(null);
    };
    document.addEventListener("keydown", handleKeyDown);
    return () => document.removeEventListener("keydown", handleKeyDown);
  }, [selected]);

  useEffect(() => {
    async function load() {
      try {
        const res = await fetch("/api/shipnoise-issues");
        if (!res.ok)
          throw new Error(`Request failed with status ${res.status}`);
        const data = await res.json();
        const issues = Array.isArray(data) ? data : (data?.data ?? []);
        const sorted = [...issues].sort(
          (a, b) => getIssueTimestamp(b) - getIssueTimestamp(a),
        );
        setIssues(sorted);
      } catch (err) {
        console.error("Fetch error:", err);
        setError(true);
      } finally {
        setLoading(false);
      }
    }
    load();
  }, []);

  if (loading) {
    return (
      <Box
        sx={{
          minHeight: "60vh",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <Typography sx={{ color: "#475569", fontSize: "18px" }}>
          Loading data...
        </Typography>
      </Box>
    );
  }

  if (error) {
    return (
      <Box
        sx={{
          minHeight: "60vh",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <Typography sx={{ color: "#ef4444", fontSize: "18px" }}>
          Failed to load data. Please try again later.
        </Typography>
      </Box>
    );
  }

  return (
    <>
      <Head>
        <title>Shipnoise — Issue Reports</title>
      </Head>

      {/* Sub-header */}
      <Box sx={{ bgcolor: "#1f2937", py: 2, px: { xs: 2, sm: 3 } }}>
        <Stack
          direction={{ xs: "column", sm: "row" }}
          alignItems={{ sm: "center" }}
          justifyContent={{ sm: "space-between" }}
          spacing={1}
          sx={{ maxWidth: "90rem", mx: "auto" }}
        >
          <Box
            component={Link}
            href="/shipnoise"
            sx={{
              color: "white",
              textDecoration: "none",
              fontWeight: 600,
              fontSize: "16px",
              "&:hover": { textDecoration: "underline" },
            }}
          >
            ← Back to Shipnoise
          </Box>
          <Stack spacing={0} alignItems={{ xs: "flex-start", sm: "flex-end" }}>
            <Typography
              sx={{ color: "white", fontWeight: 600, fontSize: "20px" }}
            >
              Issue Report Dashboard
            </Typography>
            <Typography sx={{ color: "#9ca3af", fontSize: "14px" }}>
              Total: {issues.length} submissions
            </Typography>
          </Stack>
        </Stack>
      </Box>

      <Box sx={{ bgcolor: "#f1f5f9", minHeight: "calc(100vh - 200px)", py: 5 }}>
        <Container maxWidth="lg">
          <Paper
            elevation={0}
            sx={{
              overflow: "hidden",
              borderRadius: "16px",
              border: "1px solid #e2e8f0",
              bgcolor: "white",
              boxShadow: "0 12px 24px rgba(15,23,42,0.08)",
            }}
          >
            <Box sx={{ overflowX: "auto" }}>
              <Table sx={{ minWidth: 720 }}>
                <TableHead>
                  <TableRow sx={{ bgcolor: "#f1f5f9" }}>
                    {[
                      "Submission ID",
                      "Submitted At",
                      "Nature of Error",
                      "Action",
                    ].map((h, i) => (
                      <TableCell
                        key={h}
                        align={i === 3 ? "center" : "left"}
                        sx={{
                          px: { xs: 2, sm: 3 },
                          py: 1.5,
                          fontWeight: 600,
                          color: "#475569",
                        }}
                      >
                        {h}
                      </TableCell>
                    ))}
                  </TableRow>
                </TableHead>
                <TableBody>
                  {issues.map((issue, i) => (
                    <TableRow
                      key={i}
                      hover
                      onClick={() => setSelected(issue)}
                      sx={{ cursor: "pointer" }}
                    >
                      <TableCell sx={{ px: { xs: 2, sm: 3 }, py: 1.5 }}>
                        {toDisplayString(
                          issue["Submission ID"] ?? issue.ID ?? i + 1,
                        )}
                      </TableCell>
                      <TableCell sx={{ px: { xs: 2, sm: 3 }, py: 1.5 }}>
                        {formatDateValue(
                          issue["Submitted at"] ?? issue.Timestamp ?? null,
                        )}
                      </TableCell>
                      <TableCell sx={{ px: { xs: 2, sm: 3 }, py: 1.5 }}>
                        {toDisplayString(
                          issue[
                            "What is the nature of the error you are reporting?"
                          ] ?? "(no title)",
                        )}
                      </TableCell>
                      <TableCell
                        align="center"
                        sx={{ px: { xs: 2, sm: 3 }, py: 1.5 }}
                      >
                        <Button
                          variant="contained"
                          onClick={(e) => {
                            e.stopPropagation();
                            setSelected(issue);
                          }}
                          sx={{
                            borderRadius: "999px",
                            bgcolor: "#1f2937",
                            fontSize: "12px",
                            fontWeight: 600,
                            textTransform: "none",
                            px: 3,
                            py: 0.75,
                            "&:hover": { bgcolor: "#334155" },
                          }}
                        >
                          View
                        </Button>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </Box>
          </Paper>

          {/* Modal */}
          {selected && (
            <Box
              role="dialog"
              aria-modal="true"
              aria-label="Issue Details"
              ref={dialogRef}
              tabIndex={-1}
              sx={{
                position: "fixed",
                inset: 0,
                zIndex: 50,
                display: "flex",
                alignItems: "center",
                justifyContent: "center",
                bgcolor: "rgba(0,0,0,0.4)",
                px: { xs: 2, sm: 3 },
                py: 3,
                backdropFilter: "blur(6px)",
                outline: "none",
              }}
            >
              <Paper
                sx={{
                  width: "100%",
                  maxWidth: "48rem",
                  maxHeight: "90vh",
                  overflowY: "auto",
                  borderRadius: "16px",
                  p: { xs: 2, sm: 3 },
                  boxShadow: "0 24px 48px rgba(15,23,42,0.25)",
                }}
              >
                <Box
                  component="header"
                  sx={{
                    display: "flex",
                    flexDirection: { xs: "column", sm: "row" },
                    gap: 2,
                    alignItems: { sm: "center" },
                    justifyContent: { sm: "space-between" },
                    borderBottom: "1px solid #e2e8f0",
                    pb: 1.5,
                  }}
                >
                  <Box>
                    <Typography
                      sx={{
                        fontSize: { xs: "18px", sm: "20px" },
                        fontWeight: 600,
                        color: "#0f172a",
                      }}
                    >
                      Issue Details —{" "}
                      {toDisplayString(selected["Submission ID"])}
                    </Typography>
                    <Typography sx={{ fontSize: "16px", color: "#64748b" }}>
                      Submitted:{" "}
                      {formatDateValue(
                        selected["Submitted at"] ?? selected.Timestamp ?? null,
                      )}
                    </Typography>
                  </Box>
                  <Button
                    onClick={() => setSelected(null)}
                    variant="outlined"
                    sx={{
                      alignSelf: { xs: "flex-start", sm: "auto" },
                      borderRadius: "999px",
                      borderColor: "#cbd5e1",
                      color: "#334155",
                      textTransform: "none",
                      "&:hover": {
                        bgcolor: "#f1f5f9",
                        borderColor: "#cbd5e1",
                      },
                    }}
                  >
                    Close
                  </Button>
                </Box>

                <Box
                  sx={{
                    mt: 2.5,
                    display: "grid",
                    gap: 2,
                    gridTemplateColumns: { xs: "1fr", sm: "1fr 1fr" },
                  }}
                >
                  {buildDisplayFields(selected).map(({ label, content }) => (
                    <Paper
                      key={label}
                      variant="outlined"
                      sx={{
                        borderRadius: "12px",
                        borderColor: "#f1f5f9",
                        bgcolor: "#f8fafc",
                        p: 2,
                        fontSize: "14px",
                      }}
                    >
                      <Typography
                        sx={{
                          fontSize: "11px",
                          fontWeight: 600,
                          textTransform: "uppercase",
                          letterSpacing: "0.08em",
                          color: "#94a3b8",
                        }}
                      >
                        {label}
                      </Typography>
                      <Box
                        sx={{
                          mt: 0.5,
                          color: "#1e293b",
                          wordBreak: "break-word",
                        }}
                      >
                        {content}
                      </Box>
                    </Paper>
                  ))}
                </Box>
              </Paper>
            </Box>
          )}
        </Container>
      </Box>
    </>
  );
};

ShipnoiseReportPage.getLayout = function getLayout(page: ReactElement) {
  return getShipnoiseLayout(page);
};

export default ShipnoiseReportPage;
