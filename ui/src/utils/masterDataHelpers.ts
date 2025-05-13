import { Feed } from "@/graphql/generated";

export function constructUrl(endpoint: string, paramsObj: object) {
  let params = "";
  const entries = Object.entries(paramsObj);
  for (const [key, value] of entries) {
    const str = [key, value].join("=") + "&";
    params += str;
  }
  return endpoint + "?" + params;
}

export default function formatDuration(startOffset: number, endOffset: number) {
  const seconds = endOffset - startOffset;
  const minutesRound = Math.round(seconds / 60);
  const minutesDown = Math.floor(seconds / 60);
  const remainder = Math.round(seconds % 60);
  if (seconds === 0) {
    return "audio unavailable";
  } else if (seconds < 60) {
    return `${seconds} second${seconds === 1 ? "" : "s"}`;
  } else if (seconds < 600) {
    return `${minutesDown} minute${minutesDown === 1 ? "" : "s"} ${remainder} second${remainder === 1 ? "" : "s"}`;
  } else {
    return `${minutesRound} minutes`;
  }
}

export const standardizeFeedName = (name: string) => {
  switch (name) {
    case "Beach Camp at Sunset Bay":
      return "Sunset Bay";
    case "North SJC":
      return "North San Juan Channel";
    case "Haro Strait":
      return "Orcasound Lab";
    default:
      return name;
  }
};

export const lookupFeedName = (id: string, feedList: Feed[]) => {
  let name = "feed name not found";
  feedList.forEach((feed) => {
    if (id === feed.id) {
      name = feed.name;
    }
  });
  return standardizeFeedName(name);
};

export const lookupFeedId = (name: string, feedList: Feed[]) => {
  let id = "feed id not found";
  const standardizedName = standardizeFeedName(name);
  feedList.forEach((feed) => {
    const feedName = standardizeFeedName(feed.name);
    if (standardizedName === feedName) {
      id = feed.id;
    }
  });
  return id;
};

const now = new Date();
const todayUTC = {
  yyyy: now.getUTCFullYear(),
  mm: String(now.getUTCMonth() + 1).padStart(2, "0"), // e.g. month as "05"
  dd: String(now.getUTCDate()).padStart(2, "0"),
};
export const apiTodayUTC = `${todayUTC.yyyy}-${todayUTC.mm}-${todayUTC.dd}`;
