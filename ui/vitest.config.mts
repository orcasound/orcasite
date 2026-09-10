import tsconfigPaths from "vite-tsconfig-paths";
import { defineConfig } from "vitest/config";

export default defineConfig({
  // Reads `paths` straight from tsconfig.json so they don't drift
  plugins: [tsconfigPaths()],

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
  },
});
