import { formatTimestamp, roundToNearest } from "./time";

const MINUTE = 60_000;
const FIFTEEN_MINUTES = 15 * MINUTE;

describe("roundToNearest", () => {
  it("rounds to the nearest minute", () => {
    const date = new Date("2024-03-01T12:00:29.000Z");
    expect(roundToNearest(date, MINUTE, "round").toISOString()).toBe(
      "2024-03-01T12:00:00.000Z",
    );
    expect(
      roundToNearest(new Date("2024-03-01T12:00:31.000Z"), MINUTE, "round"),
    ).toEqual(new Date("2024-03-01T12:01:00.000Z"));
  });

  it("rounds a value exactly halfway up", () => {
    // Math.round breaks ties toward +Infinity, so 30s past the minute goes up.
    expect(
      roundToNearest(new Date("2024-03-01T12:00:30.000Z"), MINUTE, "round"),
    ).toEqual(new Date("2024-03-01T12:01:00.000Z"));
  });

  it("ceils and floors in the directions their names imply", () => {
    const date = new Date("2024-03-01T12:00:01.000Z");
    expect(roundToNearest(date, MINUTE, "ceil")).toEqual(
      new Date("2024-03-01T12:01:00.000Z"),
    );
    expect(roundToNearest(date, MINUTE, "floor")).toEqual(
      new Date("2024-03-01T12:00:00.000Z"),
    );
  });

  it("leaves a value already on the unit alone, whichever method is used", () => {
    const onTheMinute = new Date("2024-03-01T12:00:00.000Z");
    for (const method of ["round", "ceil", "floor"] as const) {
      expect(roundToNearest(onTheMinute, MINUTE, method)).toEqual(onTheMinute);
    }
  });

  it("rounds to units larger than a minute", () => {
    expect(
      roundToNearest(
        new Date("2024-03-01T12:07:00.000Z"),
        FIFTEEN_MINUTES,
        "floor",
      ),
    ).toEqual(new Date("2024-03-01T12:00:00.000Z"));
    expect(
      roundToNearest(
        new Date("2024-03-01T12:08:00.000Z"),
        FIFTEEN_MINUTES,
        "round",
      ),
    ).toEqual(new Date("2024-03-01T12:15:00.000Z"));
  });

  it("handles dates before the epoch", () => {
    expect(
      roundToNearest(new Date("1969-12-31T23:59:30.000Z"), MINUTE, "round"),
    ).toEqual(new Date("1970-01-01T00:00:00.000Z"));
  });
});

describe("formatTimestamp", () => {
  // These tests compare against toLocaleString rather than against a literal like
  // "03/01/24, 12:00:00 PM UTC", because formatTimestamp passes no locale and so
  // follows whatever the machine is set to. A literal would only pass on some machines.
  const options: Intl.DateTimeFormatOptions = {
    year: "2-digit",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    timeZoneName: "short",
  };

  it("accepts a string and a Date interchangeably", () => {
    const iso = "2024-03-01T12:00:00.000Z";
    expect(formatTimestamp(iso)).toBe(formatTimestamp(new Date(iso)));
  });

  it("formats with the full set of date and time fields", () => {
    const date = new Date("2024-03-01T12:00:00.000Z");
    expect(formatTimestamp(date)).toBe(date.toLocaleString(undefined, options));
  });

  it("includes a timezone designator", () => {
    // Checks that a zone is present by comparing against the same format without one,
    // rather than matching the zone text. A short zone name isn't always letters: in a
    // half-hour-offset zone it comes out as "GMT+5:30".
    const date = new Date("2024-03-01T12:00:00.000Z");
    const { timeZoneName: _omitted, ...withoutZone } = options;

    expect(formatTimestamp(date)).not.toBe(
      date.toLocaleString(undefined, withoutZone),
    );
  });
});
