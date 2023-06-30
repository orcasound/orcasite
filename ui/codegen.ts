import { CodegenConfig } from '@graphql-codegen/cli'

const config: CodegenConfig = {
  schema: 'http://localhost:4000/graphql',
  documents: [
    'src/**/*.{graphql,js,ts,jsx,tsx}',
    '!src/graphql/generated/**/*',
  ],
  ignoreNoDocuments: true,
  hooks: { afterAllFileWrite: ['prettier --write'] },
  generates: {
    './src/graphql/generated/index.ts': {
      plugins: [
        {
          add: {
            content:
              "import { endpointUrl, fetchParams } from '@/graphql/client';",
          },
        },
        'typescript',
        'typescript-operations',
        'typescript-react-query',
      ],
      config: {
        fetcher: {
          endpoint: 'endpointUrl',
          fetchParams: 'fetchParams',
        },
        exposeDocument: true,
        exposeFetcher: true,
        exposeQueryKeys: true,
        exposeMutationKeys: true,
      },
    },
  },
}

export default config
