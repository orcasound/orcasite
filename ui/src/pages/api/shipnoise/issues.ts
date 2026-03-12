import type { NextApiRequest, NextApiResponse } from "next";

const SHEET_API_URL =
  "https://script.google.com/macros/s/AKfycbx6kn3zYIzmLLVXEAhJxW7jna-QsRwSJgvSIZvvaQOz9gvnC97tdgeXuL0MtzvET_qD/exec";

export default async function handler(
  _req: NextApiRequest,
  res: NextApiResponse,
) {
  try {
    const response = await fetch(SHEET_API_URL, {
      cache: "no-store",
    } as RequestInit);

    if (!response.ok) {
      throw new Error(`Upstream request failed with ${response.status}`);
    }

    const rawBody = await response.text();

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
    console.error("Failed to fetch Sheet data:", error);
    return res.status(500).json({ error: "Failed to fetch Sheet data" });
  }
}
