import type { NextApiRequest, NextApiResponse } from "next";

// Server-side env var (not exposed to browser)
const SHIPNOISE_API_URL =
  process.env.SHIPNOISE_API_URL || "https://orca-shipnoise-sjdtow.fly.dev";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  const { path, ...query } = req.query;

  // Reconstruct the upstream path
  const pathStr = Array.isArray(path) ? path.join("/") : (path ?? "");

  // Forward all remaining query params
  const queryString = new URLSearchParams(
    Object.entries(query).flatMap(([k, v]) =>
      Array.isArray(v) ? v.map((val) => [k, val]) : [[k, v ?? ""]],
    ),
  ).toString();

  const upstreamUrl = `${SHIPNOISE_API_URL}/${pathStr}${queryString ? `?${queryString}` : ""}`;

  try {
    const response = await fetch(upstreamUrl, {
      method: req.method,
      headers: {
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body:
        req.method !== "GET" && req.method !== "HEAD"
          ? JSON.stringify(req.body)
          : undefined,
    });

    const data = await response.json();
    return res.status(response.status).json(data);
  } catch (error) {
    console.error("Shipnoise proxy error:", error);
    return res.status(502).json({ error: "Failed to reach shipnoise backend" });
  }
}
