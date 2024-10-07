export const getAudioBaseUrlFromBucket = (bucket: string) =>
  `https://${bucket}.s3.amazonaws.com`;

export const getNodeRootUrl = (audioBaseUrl: string, nodeName: string) =>
  `${audioBaseUrl}/${nodeName}`;

export const getLatestTimestampUrl = (nodeRootUrl: string) =>
  `${nodeRootUrl}/latest.txt`;

export const getHlsUrl = (nodeRootUrl: string, timestamp: number) =>
  `${nodeRootUrl}/hls/${timestamp}/live.m3u8`;
