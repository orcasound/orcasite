// Not an ESLint config -- see eslint.config.mjs for that.
//
// ESLint 9 no longer exposes a flat-config-aware way to enumerate files, so the
// `import/no-unused-modules` rule falls back to the legacy FileEnumerator, which
// refuses to run unless it finds an eslintrc. This stub satisfies it; `root`
// stops it from searching parent directories.
// https://github.com/import-js/eslint-plugin-import/issues/3079
module.exports = { root: true };
