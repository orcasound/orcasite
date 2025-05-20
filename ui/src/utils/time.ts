export const formatTimestamp = (timestamp: string | Date) => {
  const date = new Date(timestamp);

  const options: Intl.DateTimeFormatOptions = {
    year: "2-digit",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    timeZoneName: "short",
  };

  return date.toLocaleString(undefined, options);
};

// Round to nearest time unit e.g.:
// nearest minute (timeUnitMs === 60000)
// nearest 5 minutes (timeUnitMs === 300000)
// nearest 15 minutes (timeUnitMs === 900000)
export const roundToNearest = (
  date: Date,
  timeUnitMs: number,
  roundMethod: "round" | "ceil" | "floor",
): Date => {
  const roundingFn = {
    round: Math.round,
    ceil: Math.ceil,
    floor: Math.floor,
  }[roundMethod];

  const timestamp = date.getTime();
  const roundedTimestamp = roundingFn(timestamp / timeUnitMs) * timeUnitMs;

  return new Date(roundedTimestamp);
};
