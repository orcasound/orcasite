const eslintCommand = "eslint --fix --no-warn-ignored";

const prettierCommand = "prettier --ignore-unknown --write";

module.exports = {
  "*.{js,jsx,mjs,cjs,ts,tsx,mts,cts}": [eslintCommand, prettierCommand],
  "!*.{js,jsx,mjs,cjs,ts,tsx,mts,cts}": prettierCommand,
};
