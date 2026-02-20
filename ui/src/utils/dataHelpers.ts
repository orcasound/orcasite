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
  const hoursDown = Math.floor(seconds / 60 / 60);
  const daysDown = Math.floor(seconds / 60 / 60 / 24);
  const remainder = Math.round(seconds % 60);

  if (seconds === 0) {
    return "audio unavailable";
  } else if (seconds < 60) {
    return `${seconds} second${seconds === 1 ? "" : "s"}`;
  } else if (seconds < 600) {
    return `${minutesDown} minute${minutesDown === 1 ? "" : "s"} ${remainder} second${remainder === 1 ? "" : "s"}`;
  } else if (seconds >= 600 && seconds < 3600) {
    return `${minutesRound} minute${minutesRound === 1 ? "" : "s"}`;
  } else if (seconds >= 3600 && seconds < 86400) {
    return `${hoursDown} hour${hoursDown === 1 ? "" : "s"}`;
  } else if (seconds >= 86400) {
    return `${daysDown} day${daysDown === 1 ? "" : "s"}`;
  }
}

export const cleanSightingsDescription = (
  description: string | null | undefined,
) => {
  if (!description) return "No description provided";
  // const removeBracket = description.replace(/^\[[^\]]*\]\s*/, "");
  const removeBreak = description.replace(/<br>[^•]*/g, "");
  const removeLinks = removeBreak
    .replace(/https?:\/\/\S+/g, "")
    .replace(/\s+•/g, " •")
    .trim();

  return removeLinks.trim();
};

export const standardizeFeedName = (name: string) => {
  switch (name) {
    case "Beach Camp at Sunset Bay":
      return "Sunset Bay";
    case "North SJC":
      return "North San Juan Channel";
    case "Haro Strait":
      return "Orcasound Lab";
    // case "out of range":
    //   return "Out of audible range";
    default:
      return name;
  }
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

export const lookupFeedName = (id: string, feedList: Feed[]) => {
  let name = "feed name not found";
  feedList.forEach((feed) => {
    if (id === feed.id) {
      name = feed.name;
    }
  });
  return standardizeFeedName(name);
};

export const lookupFeedSlug = (id: string, feedList: Feed[]) => {
  let slug = "feed slug not found";
  feedList.forEach((feed) => {
    if (id === feed.id) {
      slug = feed.slug;
    }
  });
  return slug;
};

const now = new Date();
const todayUTC = {
  yyyy: now.getUTCFullYear(),
  mm: String(now.getUTCMonth() + 1).padStart(2, "0"), // e.g. month as "05"
  dd: String(now.getUTCDate()).padStart(2, "0"),
};
export const apiTodayUTC = `${todayUTC.yyyy}-${todayUTC.mm}-${todayUTC.dd}`;

export const rangeOptions = {
  allTime: -1,
  sevenDays: 7 * 24 * 60 * 60 * 1000,
  threeDays: 3 * 24 * 60 * 60 * 1000,
  oneDay: 24 * 60 * 60 * 1000,
  customRange: -2,
};

export const getDateMsAgo = (durationMs: number, nowMs = Date.now()) => {
  return new Date(nowMs - durationMs);
};
