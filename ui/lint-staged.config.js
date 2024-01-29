// lint-staged configuration for next 12
// from https://nextjs.org/docs/basic-features/eslint#lint-staged
const path = require('path')

const buildEslintCommand = (filenames) =>
  `next lint --fix --file ${filenames
    .map((f) => path.relative(process.cwd(), f))
    .join(' --file ')}`

module.exports = {
  '*.{js,jsx,ts,tsx}': [buildEslintCommand], // eslint
  '*': 'prettier --ignore-unknown --write', // prettier
}