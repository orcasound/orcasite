import { afterEach, describe, expect, it, vi } from "vitest";

import {
  boutPreview,
  candidatePreview,
  feedPreview,
  firstSentence,
  formatDuration,
  formatTimeRange,
  siteOrigin,
} from "./sharePreview";

afterEach(() => {
  vi.unstubAllEnvs();
});

describe("siteOrigin", () => {
  it("adds https to the serving app's host", () => {
    vi.stubEnv("HOST_URL", "live.orcasound.net");
    expect(siteOrigin()).toBe("https://live.orcasound.net");
  });

  it("keeps a scheme that is already there", () => {
    vi.stubEnv("HOST_URL", "http://localhost:4000");
    expect(siteOrigin()).toBe("http://localhost:4000");
  });

  it("falls back to the local Next server", () => {
    vi.stubEnv("HOST_URL", "");
    vi.stubEnv("UI_PORT", "3111");
    expect(siteOrigin()).toBe("http://localhost:3111");
  });
});

describe("formatTimeRange", () => {
  it("gives Pacific time, whatever the server's time zone", () => {
    expect(
      formatTimeRange("2026-11-21T23:40:00Z", "2026-11-21T23:40:20Z"),
    ).toBe("Nov 21, 2026, 3:40 PM PST");
  });

  it("gives a range within a day once, with daylight time in summer", () => {
    expect(
      formatTimeRange("2025-08-01T08:02:51Z", "2025-08-01T09:12:26Z"),
    ).toBe("Aug 1, 2025, 1:02 – 2:12 AM PDT");
  });

  it("uses only plain spaces", () => {
    expect(
      formatTimeRange("2025-08-01T08:02:51Z", "2025-08-01T21:12:26Z"),
    ).not.toMatch(/[  ]/);
  });
});

describe("formatDuration", () => {
  it.each([
    ["2025-01-01T00:00:00Z", "2025-01-01T00:00:20Z", "1 minute"],
    ["2025-01-01T00:00:00Z", "2025-01-01T00:29:10Z", "29 minutes"],
    ["2025-01-01T00:00:00Z", "2025-01-01T01:00:00Z", "1 hour"],
    ["2025-01-01T00:00:00Z", "2025-01-01T01:49:18Z", "1 hour 49 minutes"],
    ["2025-01-01T00:00:00Z", "2025-01-01T03:01:00Z", "3 hours 1 minute"],
  ])("from %s to %s is %s", (start, end, expected) => {
    expect(formatDuration(start, end)).toBe(expected);
  });
});

describe("firstSentence", () => {
  it("takes the first sentence of the intro as plain text", () => {
    expect(
      firstSentence(
        "<p>Located near San Juan County Park, between Orcasound Lab &amp; Lime Kiln.</p>\n<p>More.</p>",
      ),
    ).toBe(
      "Located near San Juan County Park, between Orcasound Lab & Lime Kiln.",
    );
  });

  it("keeps a sentence with no closing period", () => {
    expect(firstSentence("<p>Listen here</p>")).toBe("Listen here");
  });

  it("shortens a long sentence at a word", () => {
    const sentence = firstSentence(`<p>${"word ".repeat(80)}end.</p>`);
    expect(sentence!.length).toBeLessThanOrEqual(200);
    expect(sentence).toMatch(/word…$/);
  });

  it("gives nothing for an empty intro", () => {
    expect(firstSentence(null)).toBeUndefined();
    expect(firstSentence("<p> </p>")).toBeUndefined();
  });
});

describe("feedPreview", () => {
  it("describes the feed from its intro, with its photo on the card", () => {
    const preview = feedPreview({
      slug: "andrews-bay",
      name: "Andrews Bay",
      introHtml: "<p>Located near San Juan County Park.</p>",
      imageUrl: null,
    });
    expect(preview).toMatchObject({
      title: "Listen live: Andrews Bay",
      description: "Located near San Juan County Park.",
      path: "/listen/andrews-bay",
      imagePath: "/api/og/feed/andrews-bay",
    });
    expect(preview.card.thumbUrl).toBeNull();
  });

  it("has a description when the feed has no intro", () => {
    expect(feedPreview({ slug: "x", name: "X" }).description).toBe(
      "Listen live to the X hydrophone in the Salish Sea.",
    );
  });
});

describe("candidatePreview", () => {
  const candidate = {
    id: "cand_1",
    minTime: "2026-11-21T23:40:00Z",
    maxTime: "2026-11-21T23:52:00Z",
    category: "WHALE" as const,
    detectionCount: 5,
    feed: { name: "Orcasound Lab" },
  };

  it("says what was heard, where, by how many and when", () => {
    expect(candidatePreview(candidate)).toMatchObject({
      title: "Whale sounds reported at Orcasound Lab",
      description: "5 listener reports, Nov 21, 2026, 3:40 – 3:52 PM PST.",
      path: "/reports/cand_1",
      imagePath: "/api/og/report/cand_1",
    });
  });

  it("counts a single report in the singular", () => {
    expect(
      candidatePreview({ ...candidate, detectionCount: 1 }).description,
    ).toMatch(/^1 listener report,/);
  });

  it("has a title when the report has no category", () => {
    expect(candidatePreview({ ...candidate, category: null }).title).toBe(
      "Sounds reported at Orcasound Lab",
    );
  });
});

describe("boutPreview", () => {
  const bout = {
    id: "bout_1",
    name: null,
    category: "BIOPHONY" as const,
    startTime: "2025-08-01T08:02:51Z",
    endTime: "2025-08-01T09:12:26Z",
    feed: { name: "Orcasound Lab", imageUrl: "https://example.com/a.png" },
  };

  it("names an unnamed bout by its category and feed", () => {
    expect(boutPreview(bout)).toMatchObject({
      title: "Marine life at Orcasound Lab",
      description:
        "Marine life heard at Orcasound Lab, Aug 1, 2025, 1:02 – 2:12 AM PDT, 1 hour 10 minutes.",
      path: "/bouts/bout_1",
      card: { thumbUrl: "https://example.com/a.png" },
    });
  });

  it("uses the bout's own name when it has one", () => {
    expect(
      boutPreview({ ...bout, name: "Nightime Biggs southbound in Haro" }).title,
    ).toBe("Nightime Biggs southbound in Haro");
  });

  it("leaves out the length of a bout that hasn't ended", () => {
    expect(boutPreview({ ...bout, endTime: null }).description).toBe(
      "Marine life heard at Orcasound Lab, Aug 1, 2025, 1:02 AM PDT.",
    );
  });
});
