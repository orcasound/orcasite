type Page<T> = { results: T[]; hasNextPage: boolean };

/**
 * Fetches every page of a list that the server returns a page at a time.
 *
 * `fetchPage` must return its results in a fixed order, or pages can repeat
 * or skip items. `maxPages` stops a server that always reports another page
 * from being asked for ever.
 */
export async function fetchAllPages<T>(
  fetchPage: (offset: number, limit: number) => Promise<Page<T>>,
  { pageSize = 1000, maxPages = 50 } = {},
): Promise<T[]> {
  const all: T[] = [];
  for (let page = 0; page < maxPages; page++) {
    const { results, hasNextPage } = await fetchPage(all.length, pageSize);
    all.push(...results);
    if (!hasNextPage || results.length === 0) break;
  }
  return all;
}
