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
    './src/graphql/generated/': {
      preset: 'client',
      // TODO: switch to TypedDocumentString
      // config: {
      //   documentMode: 'string',
      // },
    },
  },
}

export default config
