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
// _document.tsx loads this synchronously from <Head>, ahead of Next's deferred
// bundles, so getRuntimeConfig() sees the values by the time app code executes.
export default function handler(_req: NextApiRequest, res: NextApiResponse) {
  const config = serverRuntimeConfig();

  res.setHeader("Content-Type", "application/javascript; charset=utf-8");
  // Short-lived: a promoted slug must pick up its new app's values quickly, but
  // this blocks first paint, so it should not be refetched on every navigation.
  res.setHeader("Cache-Control", "public, max-age=60, must-revalidate");

  // JSON.stringify escapes the values; guard the one sequence that could still
  // break out of an inline script context if a value ever contains it.
  const serialized = JSON.stringify(config).replace(/</g, "\\u003c");

  res.status(200).send(`window.${RUNTIME_CONFIG_GLOBAL}=${serialized};`);
}
