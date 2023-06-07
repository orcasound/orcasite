import { CodegenConfig } from '@graphql-codegen/cli'

const config: CodegenConfig = {
  schema: 'http://localhost:4000/graphql',
  documents: [
    'src/**/*.{graphql,js,ts,jsx,tsx}',
    '!src/graphql/generated/**/*',
  ],
  // ignoreNoDocuments: true,
  hooks: { afterAllFileWrite: ['prettier --write'] },
  generates: {
    // './src/graphql/generated/': {
    //   preset: 'client',
    //   // TODO: switch to TypedDocumentString
    //   // config: {
    //   //   documentMode: 'string',
    //   // },
    // },
    // './src/graphql/generated/sdk.ts': {
    //   plugins: [
    //     'typescript',
    //     'typescript-operations',
    //     'typescript-graphql-request',
    //   ],
    //   // config: {
    //   //   documentMode: 'external',
    //   //   importDocumentNodeExternallyFrom: './graphql',
    //   //   importOperationTypesFrom: 'Operations',
    //   // },
    // },
    './src/graphql/generated/types.ts': {
      plugins: [
        'typescript',
        'typescript-operations',
        'typescript-react-query',
      ],
      config: {
        fetcher: 'graphql-request',
      },
    },
  },
}

export default config
