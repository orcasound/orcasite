/* eslint-disable import/no-unused-modules */

// Duplicated rather than imported from utils/runtimeConfig -- see the note
// there. The minifier can only strip the NEXT_PUBLIC_* reads below when this
// folds to false within this module; an imported binding is opaque to it.
const allowInlinedConfig =
  process.env.NODE_ENV === "development" ||
  process.env.NEXT_PUBLIC_ALLOW_INLINED_CONFIG === "true";

// Resolve the endpoint per call rather than at module load. Next inlines
// NEXT_PUBLIC_* into the client bundle at build time, so a hardcoded absolute
// host survives Heroku slug promotion: a bundle built in one app keeps querying
// that app's API after being promoted to another.
//
// In the browser the API is same-origin -- Phoenix is the public entry point and
// reverse-proxies / to Next (see server/lib/orcasite_web/router.ex), so /graphql
// is served by whichever host served the page. Local dev is the exception, with
// Next on :3000 and Phoenix on :4000, so NEXT_PUBLIC_GQL_ENDPOINT wins there.
//
// That override is gated on allowInlinedConfig. NEXT_PUBLIC_* was the old
// mechanism, so setting one as a config var on the build app is an easy mistake
// to make; in a deployed build it is ignored rather than silently pointing every
// promoted environment back at the app the bundle was built in.
export function endpointUrl(): string {
  if (typeof window !== "undefined") {
    if (allowInlinedConfig && process.env.NEXT_PUBLIC_GQL_ENDPOINT) {
      return process.env.NEXT_PUBLIC_GQL_ENDPOINT;
    }

    return "/graphql";
  }

  // Server-side rendering can't use a relative URL. GQL_ENDPOINT lacks the
  // NEXT_PUBLIC_ prefix, so it stays a runtime lookup instead of becoming a
  // build-time constant; server/load_env.sh derives it from the app's own
  // HOST_URL at dyno boot.
  const serverEndpoint =
    process.env.GQL_ENDPOINT ||
    (allowInlinedConfig ? process.env.NEXT_PUBLIC_GQL_ENDPOINT : undefined);

  if (!serverEndpoint) {
    throw new Error("GQL_ENDPOINT is not set");
  }

  return serverEndpoint;
}

type GraphQLError = {
  message: string;
  path?: (string | number)[];
};

/**
 * A well-formed GraphQL response that carried errors.
 *
 * Distinct from a transport failure (fetch rejecting, a non-JSON body), so
 * callers can tell "the API answered, and the answer was an error" from "the
 * API could not be reached" -- which matter differently.
 */
export class GraphQLResponseError extends Error {
  readonly errors: GraphQLError[];

  constructor(errors: GraphQLError[]) {
    super(errors[0]?.message ?? "GraphQL request failed");
    this.name = "GraphQLResponseError";
    this.errors = errors;
  }
}

/**
 * Whether an error means the requested record does not exist.
 *
 * The schema marks these lookups non-nullable, so a missing record surfaces as
 * `data: null` plus "Cannot return null for non-nullable field" naming the
 * field. There is no error code to key off, so match the field path and that
 * condition, and treat everything else -- network failures, timeouts, internal
 * server errors -- as what it is. Callers that turn missing records into 404s
 * must not do the same to a transient outage.
 */
export function isMissingRecordError(error: unknown, field: string): boolean {
  return (
    error instanceof GraphQLResponseError &&
    error.errors.some(
      (graphQLError) =>
        graphQLError.path?.[graphQLError.path.length - 1] === field &&
        /non-?nullable/i.test(graphQLError.message),
    )
  );
}

export const fetchParams = () => {
  return {
    headers: {
      "Content-Type": "application/json; charset=utf-8",
    },
  };
};

export function fetcher<TData, TVariables>(
  query: string,
  variables?: TVariables,
  options?: RequestInit["headers"],
) {
  return async () => {
    const res = await fetch(endpointUrl(), {
      method: "POST",
      ...fetchParams(),
      ...options,
      body: JSON.stringify({ query, variables }),
    });

    const json = await res.json();

    if (json.errors && (!json.data || Object.keys(json.data).length === 0)) {
      throw new GraphQLResponseError(json.errors);
    }

    return json.data as TData;
  };
}
