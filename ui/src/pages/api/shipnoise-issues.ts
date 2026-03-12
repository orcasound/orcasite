import type { NextApiRequest, NextApiResponse } from "next";

const SHEET_API_URL =
  "https://script.google.com/macros/s/AKfycbx6kn3zYIzmLLVXEAhJxW7jna-QsRwSJgvSIZvvaQOz9gvnC97tdgeXuL0MtzvET_qD/exec";

const FETCH_TIMEOUT_MS = 10000;

export default async function handler(
  _req: NextApiRequest,
  res: NextApiResponse,
) {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), FETCH_TIMEOUT_MS);

  try {
    const response = await fetch(SHEET_API_URL, {
      cache: "no-store",
      signal: controller.signal,
    } as RequestInit);

    if (!response.ok) {
      clearTimeout(timeout);
      throw new Error(`Upstream request failed with ${response.status}`);
    }

    const rawBody = await response.text();
    clearTimeout(timeout);

    try {
      const sanitized = rawBody.replace(/^\)\]\}'/, "").trim();
      if (!sanitized) {
        return res.status(200).json({ data: [] });
      }
      const parsed = JSON.parse(sanitized);
      return res.status(200).json(parsed);
    } catch (parseError) {
      console.error("Upstream returned non-JSON payload:", parseError);
      return res.status(502).json({
        error: "Upstream response was not valid JSON",
      });
    }
  } catch (error) {
    clearTimeout(timeout);
    if (error instanceof Error && error.name === "AbortError") {
      console.error("Sheet request timed out");
      return res.status(504).json({ error: "Request to Sheet timed out" });
    }
    console.error("Failed to fetch Sheet data:", error);
    return res.status(500).json({ error: "Failed to fetch Sheet data" });
  }
}
