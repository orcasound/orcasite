import { describe, expect, it, vi } from "vitest";

import { fetchAllPages } from "./fetchAllPages";

// A server holding `total` items that returns at most `limit` of them.
function server(total: number) {
  return vi.fn(async (offset: number, limit: number) => {
    const results = Array.from(
      { length: Math.max(0, Math.min(limit, total - offset)) },
      (_, i) => offset + i,
    );
    return { results, hasNextPage: offset + results.length < total };
  });
}

describe("fetchAllPages", () => {
  it("makes one request when everything fits in a page", async () => {
    const fetchPage = server(139);
    expect(await fetchAllPages(fetchPage)).toHaveLength(139);
    expect(fetchPage).toHaveBeenCalledTimes(1);
  });

  it("keeps asking until the server reports no further page", async () => {
    const fetchPage = server(1220);
    const all = await fetchAllPages(fetchPage);
    expect(all).toEqual(Array.from({ length: 1220 }, (_, i) => i));
    expect(fetchPage.mock.calls).toEqual([
      [0, 1000],
      [1000, 1000],
    ]);
  });

  it("stops at maxPages when the server always reports another page", async () => {
    const fetchPage = vi.fn(async () => ({ results: [1], hasNextPage: true }));
    expect(await fetchAllPages(fetchPage, { maxPages: 3 })).toHaveLength(3);
  });

  it("stops when a page comes back empty", async () => {
    const fetchPage = vi.fn(async () => ({ results: [], hasNextPage: true }));
    expect(await fetchAllPages(fetchPage)).toEqual([]);
    expect(fetchPage).toHaveBeenCalledTimes(1);
  });
});
