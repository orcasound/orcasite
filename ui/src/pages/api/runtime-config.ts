import type { NextApiRequest, NextApiResponse } from "next";

import {
  RUNTIME_CONFIG_GLOBAL,
  serverRuntimeConfig,
} from "@/utils/runtimeConfig";

// Serves the per-environment config as a script that defines a global before the
// app bundle runs. API routes are always rendered per request -- never statically
// optimized -- so this reads the serving app's environment even on pages that
// were prerendered at build time in a different app.
//
// _document.tsx loads this as a deferred script from <Head>. Deferred scripts
// execute in document order, so it runs ahead of Next's own deferred bundles
// and getRuntimeConfig() sees the values by the time app code executes.
export default function handler(_req: NextApiRequest, res: NextApiResponse) {
  const config = serverRuntimeConfig();

  res.setHeader("Content-Type", "application/javascript; charset=utf-8");
  // These values only change when a slug is promoted, so this is cached rather
  // than fetched per page load; 60s bounds how long a freshly promoted app
  // could serve the previous app's values.
  res.setHeader("Cache-Control", "public, max-age=60, must-revalidate");

  // JSON.stringify escapes the values; guard the one sequence that could still
  // break out of an inline script context if a value ever contains it.
  const serialized = JSON.stringify(config).replace(/</g, "\\u003c");

  res.status(200).send(`window.${RUNTIME_CONFIG_GLOBAL}=${serialized};`);
}
