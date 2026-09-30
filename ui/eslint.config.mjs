import { defineConfig, globalIgnores } from "eslint/config";
import nextCoreWebVitals from "eslint-config-next/core-web-vitals";
import prettier from "eslint-config-prettier/flat";
import simpleImportSort from "eslint-plugin-simple-import-sort";
import unusedImports from "eslint-plugin-unused-imports";
import tseslint from "typescript-eslint";

export default defineConfig([
  globalIgnores(["src/graphql/generated/**"]),

  // Includes the base `next` config (react, react-hooks, import, jsx-a11y and
  // @next/next plugins) plus the core web vitals rules as errors
  ...nextCoreWebVitals,

  {
    // Same files as the base `next` config, which is where the react-hooks and
    // import plugins these rules belong to are registered
    files: ["**/*.{js,jsx,mjs,ts,tsx,mts,cts}"],
    plugins: {
      "simple-import-sort": simpleImportSort,
      "unused-imports": unusedImports,
    },
    rules: {
      // eslint-plugin-react-hooks 7 adds React Compiler rules to its recommended
      // set. We don't use the compiler, and the existing ref and effect patterns
      // they flag would need rewriting first, so keep to the classic hooks rules
      // (rules-of-hooks, exhaustive-deps) for now.
      // TODO: Enable these when adopting the React Compiler
      ...Object.fromEntries(
        [
          "static-components",
          "use-memo",
          "preserve-manual-memoization",
          "incompatible-library",
          "immutability",
          "globals",
          "refs",
          "set-state-in-effect",
          "error-boundaries",
          "purity",
          "set-state-in-render",
          "unsupported-syntax",
          "config",
          "gating",
        ].map((rule) => [`react-hooks/${rule}`, "off"]),
      ),

      "prefer-const": "warn",
      "simple-import-sort/imports": "warn",
      "simple-import-sort/exports": "warn",
      "unused-imports/no-unused-imports": "warn",
      "unused-imports/no-unused-vars": [
        "warn",
        {
          vars: "all",
          varsIgnorePattern: "^_",
          args: "after-used",
          argsIgnorePattern: "^_",
        },
      ],
      "import/no-unused-modules": [
        "warn",
        {
          unusedExports: true,
          ignoreExports: [
            "src/pages", // pages are automatically imported by nextjs
            "codegen.ts",
            "vitest.config.mts",
            "eslint.config.mjs",
            "test", // helpers are imported by test files, which the rule scans narrowly
          ],
        },
      ],
    },
  },

  {
    files: ["**/*.{ts,tsx}"],
    extends: [
      tseslint.configs.recommended,
      // TODO: Enable strict type checking
      // tseslint.configs.strictTypeChecked,
      // tseslint.configs.stylisticTypeChecked,
    ],
    languageOptions: {
      parserOptions: {
        project: "./tsconfig.json",
        tsconfigRootDir: import.meta.dirname,
      },
    },
    rules: {
      // typescript only rules go here
      "@typescript-eslint/no-unused-vars": "off",
      "@typescript-eslint/no-empty-function": "warn",
    },
  },

  // Turns off rules that conflict with prettier; keep last
  prettier,
]);
