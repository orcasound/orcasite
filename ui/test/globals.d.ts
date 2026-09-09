// Next generates next-env.d.ts, which is what declares the types that let you import
// an SVG or PNG as a module. That file is gitignored and only appears after a
// `next dev` or `next build`, so on a fresh clone `tsc --noEmit` fails on every image
// import. Referencing the same types here fixes that without committing a generated
// file.
/// <reference types="next" />
/// <reference types="next/image-types/global" />

// Makes describe/it/expect/vi available without importing them, to match
// `globals: true` in vitest.config.mts.
//
// This has to be a reference here rather than `"types": ["vitest/globals"]` in
// tsconfig.json. Setting "types" turns off TypeScript's default of loading every
// @types package it can find, which would drop @types/node, @types/react and the rest.
/// <reference types="vitest/globals" />
