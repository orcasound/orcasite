module.exports = {
  schema: 'http://localhost:4000/graphql',
  documents: [
    'src/**/*.{graphql,js,ts,jsx,tsx}',
    '!src/graphql/generated/**/*',
  ],
}
