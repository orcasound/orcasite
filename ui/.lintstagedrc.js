// from https://nextjs.org/docs/pages/building-your-application/configuring/eslint#lint-staged
const path = require("path");

const buildEslintCommand = (filenames) =>
  `next lint --fix --file ${filenames
    .map((f) => path.relative(process.cwd(), f))
    .join(" --file ")}`;

const prettierCommand = "prettier --ignore-unknown --write";

module.exports = {
  "*.{js,jsx,ts,tsx}": [buildEslintCommand, prettierCommand],
  "!*.{js,jsx,ts,tsx}": prettierCommand,
};
