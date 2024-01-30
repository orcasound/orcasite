// from https://nextjs.org/docs/pages/building-your-application/configuring/eslint#lint-staged
const path = require("path");

const buildEslintCommand = (filenames) =>
  `next lint --fix --file ${filenames
    .map((f) => path.relative(process.cwd(), f))
    .join(" --file ")}`;

module.exports = {
  "src/*.{js,jsx,ts,tsx}": [buildEslintCommand], // eslint
  "*": "prettier --ignore-unknown --write",
};
