import { CodegenConfig } from "@graphql-codegen/cli";

// Shared by the schema types and the operation types so both render scalars and
// enums the same way
const typesConfig = {
  strictScalars: true,
  scalars: {
    // TODO: Choose a decimal library and use that type instead
    // For custom scalars config, see https://github.com/dotansimha/graphql-code-generator/issues/153
    Decimal: "number",
    DateTime: "Date",
    Json: "{ [key: string]: any }",
  },
};

const config: CodegenConfig = {
  schema: "http://localhost:4000/graphql",
  documents: [
    "src/**/*.{graphql,js,ts,jsx,tsx}",
    "!src/graphql/generated/**/*",
  ],
  ignoreNoDocuments: true,
  hooks: { afterAllFileWrite: ["prettier --write"] },
  generates: {
    // Schema object, input and enum types (e.g. `Feed`, `DetectionCategory`).
    // Since v6, typescript-operations no longer emits these itself.
    "./src/graphql/generated/schema.ts": {
      plugins: ["typescript"],
      config: { ...typesConfig, enumsAsConst: true },
    },
    "./src/graphql/generated/index.ts": {
      plugins: [
        // Re-export the schema types so everything stays importable from
        // "@/graphql/generated"
        { add: { content: 'export * from "./schema";' } },
        "typescript-operations",
        "typescript-react-query",
      ],
      config: {
        ...typesConfig,
        // Reference the schema types above instead of redeclaring them
        importSchemaTypesFrom: "./src/graphql/generated/schema.ts",
        enumType: "const",
        reactQueryVersion: 5,
        fetcher: "@/graphql/client#fetcher",
        exposeDocument: true,
        exposeFetcher: true,
        exposeQueryKeys: true,
        exposeMutationKeys: true,
        exposeSubscriptionKeys: true,
      },
    },
  },
};

export default config;
