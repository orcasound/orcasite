import tsconfigPaths from "vite-tsconfig-paths";
import { defineConfig } from "vitest/config";

export default defineConfig({
  plugins: [
    // Reads `paths` straight from tsconfig.json so they don't drift
    tsconfigPaths(),

    // Next turns an image import into { src, height, width } and Vite into a URL
    // string; this gives tests Next's shape, so components reading `.src` work
    {
      name: "next-static-image",
      enforce: "pre",
      load(id) {
        if (/\.(svg|png|jpe?g|gif|webp|avif)$/.test(id)) {
          return `export default ${JSON.stringify({ src: id, height: 1, width: 1 })}`;
        }
      },
    },
  ],

  // tsconfig.json sets `jsx: "preserve"` for Next's compiler, which would otherwise
  // leave raw JSX behind
  esbuild: { jsx: "automatic" },

  test: {
    environment: "jsdom",

    // @testing-library/react only registers its own afterEach(cleanup) when a
    // global afterEach exists. Without this, DOM leaks between tests
    globals: true,

    setupFiles: ["./test/setup.ts"],
    include: ["src/**/*.test.{ts,tsx}", "test/**/*.test.{ts,tsx}"],

    clearMocks: true,
    restoreMocks: true,

    // test/graphql.ts stubs fetch; put the real one back after each test
    unstubGlobals: true,
  },
});
