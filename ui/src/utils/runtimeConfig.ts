// Per-environment values that must NOT be baked into the client bundle.
//
// Next inlines NEXT_PUBLIC_* at build time. Because the Heroku slug is built
// once and promoted across apps, anything inlined that way describes the app it
// was built in, not the app serving the request -- that is how production ended
// up pointing at development resources. These values are instead injected at
// request time by /api/runtime-config, which reads the serving app's own
// environment. See src/pages/api/runtime-config.ts.
//
// NEXT_PUBLIC_* is honoured only in development builds, where the app is built
// and run in one environment and inlining is harmless. Outside development the
// prefixed names are ignored entirely, so adding one as a config var on the
// build app cannot silently reintroduce the bug this module exists to prevent.

type RuntimeConfig = {
  s3Bucket: string;
  gaId?: string;
};

export const RUNTIME_CONFIG_GLOBAL = "__ORCASITE_CONFIG__";

// Last resort only, when injection failed and there is nothing better to use.
// Deliberately not a real environment's bucket: silently falling back to
// production's would let a broken staging stream production audio unnoticed.
const FALLBACK_S3_BUCKET = "";

/**
 * Whether inlined NEXT_PUBLIC_* values may be used at all.
 *
 * True for `next dev`, and opt-in via NEXT_PUBLIC_ALLOW_INLINED_CONFIG for the
 * production-style local build (`npm run build:dev` / `start:dev`), which sets
 * NODE_ENV=production but still serves Next on :3000 with Phoenix on :4000 and
 * so needs the absolute endpoints from .env.development.
 *
 * A deployed build must never set the flag: leaving it off is what stops a
 * stray NEXT_PUBLIC_* config var on the build app from pinning every promoted
 * environment to the one the bundle was built in.
 *
 * NOTE: this constant is duplicated in graphql/client.ts and hooks/useSocket.ts
 * rather than shared. Next replaces process.env.* with literals per module, and
 * the minifier can only drop the guarded NEXT_PUBLIC_* reads when the condition
 * folds to false in the same module. Imported from elsewhere it is an opaque
 * binding, the branch survives, and the values land in the bundle after all --
 * which is the entire bug this file exists to prevent. Verified by building with
 * NEXT_PUBLIC_* set and grepping .next/static for the values.
 */
const allowInlinedConfig =
  process.env.NODE_ENV === "development" ||
  process.env.NEXT_PUBLIC_ALLOW_INLINED_CONFIG === "true";

declare global {
  interface Window {
    [RUNTIME_CONFIG_GLOBAL]?: Partial<RuntimeConfig>;
  }
}

let warnedMissingConfig = false;

/**
 * Resolve config for the environment currently serving the page.
 *
 * Must be called lazily -- at render or event time, never at module scope --
 * so the browser reads the injected globals rather than whatever was present
 * when the bundle was compiled.
 */
export function getRuntimeConfig(): RuntimeConfig {
  if (typeof window !== "undefined") {
    const injected = window[RUNTIME_CONFIG_GLOBAL];

    // A failed deferred script does not stop the ones after it, so a broken
    // /api/runtime-config would otherwise degrade silently: no analytics, and a
    // bucket belonging to whichever environment supplied the fallback. Say so.
    if (!injected && !warnedMissingConfig) {
      warnedMissingConfig = true;
      console.error(
        `window.${RUNTIME_CONFIG_GLOBAL} is missing -- /api/runtime-config did not load. ` +
          "Analytics is disabled and audio URLs may be wrong until it does.",
      );
    }

    return {
      s3Bucket:
        injected?.s3Bucket ||
        (allowInlinedConfig ? process.env.NEXT_PUBLIC_S3_BUCKET : undefined) ||
        FALLBACK_S3_BUCKET,
      gaId:
        injected?.gaId ||
        (allowInlinedConfig ? process.env.NEXT_PUBLIC_GA_ID : undefined) ||
        undefined,
    };
  }

  return serverRuntimeConfig();
}

/**
 * Server-side lookup. These names are deliberately un-prefixed so Next treats
 * them as runtime reads instead of build-time constants; server/load_env.sh and
 * the Heroku config vars supply them per app.
 */
export function serverRuntimeConfig(): RuntimeConfig {
  return {
    s3Bucket:
      process.env.S3_BUCKET ||
      (allowInlinedConfig ? process.env.NEXT_PUBLIC_S3_BUCKET : undefined) ||
      FALLBACK_S3_BUCKET,
    gaId:
      process.env.GOOGLE_ANALYTICS_ID ||
      (allowInlinedConfig ? process.env.NEXT_PUBLIC_GA_ID : undefined) ||
      undefined,
  };
}
