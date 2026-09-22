// A stand-in for the GraphQL API, for tests that render components which query it.
//
// It stubs fetch, the seam every generated hook goes through (src/graphql/client.ts),
// so a test runs the real hooks and cache and only the network is fake.

// Bout's :index action, like most read actions here, sets no max_page_size, so Ash
// applies its default and AshGraphql quietly returns min(limit, 250) rows
export const ASH_DEFAULT_MAX_PAGE_SIZE = 250;

// Variables arrive as parsed JSON, so each resolver declares the type it expects.
// `never` lets resolvers with different variable types share one map.
type Resolver = (variables: never) => unknown;

/**
 * Routes each request to the resolver named after its operation: `query bouts(...)`
 * goes to `resolvers.bouts`, and whatever it returns becomes the response's `data`.
 * An operation without a resolver throws, so a missing stub fails instead of hanging.
 * The real fetch comes back after each test (`unstubGlobals` in the vitest config).
 */
export function stubGraphql(resolvers: Record<string, Resolver>) {
  const fetch = vi.fn(async (_url: string, init?: RequestInit) => {
    const { query, variables } = JSON.parse(String(init?.body));
    const operation = /^\s*(?:query|mutation)\s+(\w+)/.exec(query)?.[1] ?? "";
    const resolve = resolvers[operation];

    if (!resolve) {
      throw new Error(`No stub for GraphQL operation "${operation}"`);
    }

    return Response.json({ data: await resolve((variables ?? {}) as never) });
  });

  vi.stubGlobal("fetch", fetch);
  return fetch;
}

/**
 * One page of `rows` the way AshGraphql answers an offset-paginated read: the limit
 * capped at the action's max page size, with the total count alongside.
 */
export function offsetPage<Row>(
  rows: Row[],
  { limit, offset }: { limit: number; offset: number },
  maxPageSize = ASH_DEFAULT_MAX_PAGE_SIZE,
) {
  const pageSize = Math.min(limit, maxPageSize);

  return {
    count: rows.length,
    hasNextPage: offset + pageSize < rows.length,
    results: rows.slice(offset, offset + pageSize),
  };
}
