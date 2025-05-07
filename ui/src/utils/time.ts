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
) => {
  let roundFn;
  if (roundMethod === "round") {
    roundFn = Math.round;
  } else if (roundMethod === "ceil") {
    roundFn = Math.ceil;
  } else {
    roundFn = Math.floor;
  }

  return new Date(roundFn(date.getTime() / timeUnitMs) * timeUnitMs);
};

// // get current timestamp for PST
// export function getPacificNow(): Date {
//   const now = new Date();
//   const pacific = new Intl.DateTimeFormat("en-US", {
//     timeZone: "America/Los_Angeles",
//     year: "numeric",
//     month: "2-digit",
//     day: "2-digit",
//     hour: "2-digit",
//     minute: "2-digit",
//     second: "2-digit",
//     hour12: false,
//   }).formatToParts(now);

//   const parts: { [key: string]: string } = {};
//   for (const { type, value } of pacific) {
//     if (type !== "literal") parts[type] = value;
//   }

//   return new Date(
//     `${parts.year}-${parts.month}-${parts.day}T${parts.hour}:${parts.minute}:${parts.second}Z`
//   );
// }

// // convert UTC to PST, preserving input type (date object or string)
// type PacificTimeResult<T extends string | Date> = {
//   value: T;
//   zone: string; // e.g., "PDT" or "PST"
// };

// export function convertUtcToPacific<T extends string | Date>(
//   input: T
// ): PacificTimeResult<T> {
//   const date = input instanceof Date ? input : new Date(input);

//   const formatter = new Intl.DateTimeFormat("en-US", {
//     timeZone: "America/Los_Angeles",
//     timeZoneName: "short",
//     year: "numeric",
//     month: "2-digit",
//     day: "2-digit",
//     hour: "2-digit",
//     minute: "2-digit",
//     second: "2-digit",
//     hour12: false,
//   });

//   const parts = formatter.formatToParts(date);

//   const result: Record<string, string> = {};
//   for (const part of parts) {
//     if (part.type !== "literal") {
//       result[part.type] = part.value;
//     }
//   }

//   const pacificDateString = `${result.year}-${result.month}-${result.day}T${result.hour}:${result.minute}:${result.second}`;
//   const timezone = result.timeZoneName;

//   const output =
//     typeof input === "string"
//       ? (pacificDateString as T)
//       : (new Date(pacificDateString) as T);

//   return {
//     value: output,
//     zone: timezone,
//   };
// }
