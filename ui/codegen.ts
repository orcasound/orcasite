import { CodegenConfig } from "@graphql-codegen/cli";

const config: CodegenConfig = {
  schema: "http://localhost:4000/graphql",
  documents: [
    "src/**/*.{graphql,js,ts,jsx,tsx}",
    "!src/graphql/generated/**/*",
  ],
  ignoreNoDocuments: true,
  hooks: { afterAllFileWrite: ["prettier --write"] },
  generates: {
    "./src/graphql/generated/index.ts": {
      plugins: [
        "typescript",
        "typescript-operations",
        "typescript-react-query",
      ],
      config: {
        reactQueryVersion: 5,
        fetcher: "@/graphql/client#fetcher",
        exposeDocument: true,
        exposeFetcher: true,
        exposeQueryKeys: true,
        exposeMutationKeys: true,
        exposeSubscriptionKeys: true,
        enumsAsConst: true,
        strictScalars: true,
        scalars: {
          // TODO: Choose a decimal library and use that type instead
          // For custom scalars config, see https://github.com/dotansimha/graphql-code-generator/issues/153
          Decimal: "number",
          DateTime: "Date",
          Json: "{ [key: string]: any }",
        },
      },
    },
  },
};

export default config;
