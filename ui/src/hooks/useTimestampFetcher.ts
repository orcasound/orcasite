import { useEffect, useState } from "react";

import { getHlsUrl, getLatestTimestampUrl, getNodeRootUrl } from "@/utils/urls";

/**
 * @typedef {Object} TimestampFetcherOptions
 * @property {() => void} onStart Callback when the fetcher starts
 * @property {() => void} onStop Callback when the fetcher stops
 */

/**
 * @typedef {Object} TimestampFetcherResult
 * @property {number} timestamp The latest timestamp
 * @property {string} hlsUrl The URL to the latest HLS stream
 */

/**
 * Starts a timer that fetches the latest timestamp from a feed
 * @param {string} audioBaseUrl The base URL for the audio feed
 * @param {string} nodeName The name of the feed to fetch from
 * @param {TimestampFetcherOptions} options Callbacks for when the fetcher starts and stops
 * @returns {TimestampFetcherResult} The latest timestamp and HLS URL
 */
export function useTimestampFetcher(
  audioBaseUrl?: string,
  nodeName?: string,
  { onStart, onStop }: { onStart?: () => void; onStop?: () => void } = {},
) {
  const [timestamp, setTimestamp] = useState<number>();

  const nodeRootUrl =
    audioBaseUrl && nodeName
      ? getNodeRootUrl(audioBaseUrl, nodeName)
      : undefined;

  const hlsUrl =
    nodeRootUrl && timestamp ? getHlsUrl(nodeRootUrl, timestamp) : undefined;

  useEffect(() => {
    let currentXhr: XMLHttpRequest | undefined;
    let intervalId: NodeJS.Timeout | undefined;

    const fetchTimestamp = (timestampUrl: string) => {
      const xhr = new XMLHttpRequest();
      currentXhr = xhr;
      xhr.open("GET", timestampUrl);
      xhr.onload = () => {
        if (xhr.status === 200) {
          const newTimestamp = Number(xhr.responseText.trim());
          if (process.env.NODE_ENV === "development")
            console.log("Latest timestamp: " + newTimestamp);
          setTimestamp(newTimestamp);
        }
      };
      xhr.send();
    };

    const startFetcher = () => {
      if (!nodeRootUrl) return;
      const timestampUrl = getLatestTimestampUrl(nodeRootUrl);

      onStart?.();
      fetchTimestamp(timestampUrl);
      const newIntervalId = setInterval(
        () => fetchTimestamp(timestampUrl),
        10000,
      );
      intervalId = newIntervalId;
    };

    const stopFetcher = () => {
      if (currentXhr) currentXhr.abort();
      if (intervalId) clearInterval(intervalId);
    };

    startFetcher();

    return () => {
      stopFetcher();
      onStop?.();
    };
  }, [nodeRootUrl, onStart, onStop]);

  return { timestamp, hlsUrl };
}
