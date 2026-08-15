// Per-environment values that must NOT be baked into the client bundle.
//
// Next inlines NEXT_PUBLIC_* at build time. Because the Heroku slug is built
// once and promoted across apps, anything inlined that way describes the app it
// was built in, not the app serving the request -- that is how production ended
// up pointing at development resources. These values are instead injected at
// request time by /api/runtime-config, which reads the serving app's own
// environment. See src/pages/api/runtime-config.ts.
//
// NEXT_PUBLIC_* is still honoured as a fallback so local development, which
// builds and runs in one environment, keeps working from .env.development.

type RuntimeConfig = {
  s3Bucket: string;
  gaId?: string;
};

export const RUNTIME_CONFIG_GLOBAL = "__ORCASITE_CONFIG__";

const DEFAULT_S3_BUCKET = "audio-orcasound-net";

declare global {
  interface Window {
    [RUNTIME_CONFIG_GLOBAL]?: Partial<RuntimeConfig>;
  }
}

/**
 * Resolve config for the environment currently serving the page.
 *
 * Must be called lazily -- at render or event time, never at module scope --
 * so the browser reads the injected globals rather than whatever was present
 * when the bundle was compiled.
 */
export function getRuntimeConfig(): RuntimeConfig {
  if (typeof window !== "undefined") {
    const injected = window[RUNTIME_CONFIG_GLOBAL] ?? {};

    return {
      s3Bucket:
        injected.s3Bucket ||
        process.env.NEXT_PUBLIC_S3_BUCKET ||
        DEFAULT_S3_BUCKET,
      gaId: injected.gaId || process.env.NEXT_PUBLIC_GA_ID || undefined,
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
      process.env.NEXT_PUBLIC_S3_BUCKET ||
      DEFAULT_S3_BUCKET,
    gaId:
      process.env.GOOGLE_ANALYTICS_ID ||
      process.env.NEXT_PUBLIC_GA_ID ||
      undefined,
  };
}
