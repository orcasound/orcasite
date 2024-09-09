import { useEffect, useState } from "react";

if (!process.env.NEXT_PUBLIC_S3_BUCKET) {
  throw new Error("NEXT_PUBLIC_S3_BUCKET is not set");
}

const getBucketBase = (bucket: string) => `https://${bucket}.s3.amazonaws.com`;

const getTimestampURI = (bucket: string, nodeName: string) =>
  `${getBucketBase(bucket)}/${nodeName}/latest.txt`;

export const getHlsURI = (
  bucket: string,
  nodeName: string,
  timestamp: number,
) => `${getBucketBase(bucket)}/${nodeName}/hls/${timestamp}/live.m3u8`;

/**
 * @typedef {Object} TimestampFetcherOptions
 * @property {() => void} onStart Callback when the fetcher starts
 * @property {() => void} onStop Callback when the fetcher stops
 */

/**
 * @typedef {Object} TimestampFetcherResult
 * @property {number} timestamp The latest timestamp
 * @property {string} hlsURI The URI to the latest HLS stream
 * @property {string} awsConsoleUri The URI to the AWS console for the latest HLS stream
 */

/**
 * Starts a timer that fetches the latest timestamp from a feed
 * @param {string} bucket The bucket name of the node
 * @param {string} nodeName The name of the feed to fetch from, as defined in the S3 bucket
 * @param {TimestampFetcherOptions} options Callbacks for when the fetcher starts and stops
 * @returns {TimestampFetcherResult} The latest timestamp, HLS URI, and AWS console URI
 */
export function useTimestampFetcher(
  bucket?: string,
  nodeName?: string,
  { onStart, onStop }: { onStart?: () => void; onStop?: () => void } = {},
) {
  const [timestamp, setTimestamp] = useState<number>();

  const hlsURI =
    nodeName && bucket && timestamp
      ? getHlsURI(bucket, nodeName, timestamp)
      : undefined;
  const awsConsoleUri =
    nodeName && bucket && timestamp
      ? `https://s3.console.aws.amazon.com/s3/buckets/${bucket}/${nodeName}/hls/${timestamp}/`
      : undefined;

  useEffect(() => {
    let currentXhr: XMLHttpRequest | undefined;
    let intervalId: NodeJS.Timeout | undefined;

    const fetchTimestamp = (timestampURI: string) => {
      const xhr = new XMLHttpRequest();
      currentXhr = xhr;
      xhr.open("GET", timestampURI);
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
      if (!nodeName || !bucket) return;
      const timestampURI = getTimestampURI(bucket, nodeName);

      onStart?.();
      fetchTimestamp(timestampURI);
      const newIntervalId = setInterval(
        () => fetchTimestamp(timestampURI),
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
  }, [nodeName, onStart, onStop, bucket]);

  return { timestamp, hlsURI, awsConsoleUri };
}
