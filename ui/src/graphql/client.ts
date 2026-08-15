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
      const { message } = json.errors[0];
      throw new Error(message);
    }

    return json.data as TData;
  };
}
