export const storeCurrentFeed = currentFeed => {
  localStorage.setItem("currentFeed", JSON.stringify(currentFeed))
}

export const getCurrentFeed = () => {
  try {
    return JSON.parse(localStorage.getItem("currentFeed"))
  } catch (error) {
    localStorage.removeItem("currentFeed")
  }
}

export const feedSrc = (nodeName, timestamp) =>
  `https://s3-us-west-2.amazonaws.com/${ENV.S3_BUCKET}/${nodeName}/hls/${timestamp}/live.m3u8`
