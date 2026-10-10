// Copies the names the tag picker searches out of a salish-sea/animals register release
// (#1015): the release is a pinned publication, not a service, so the picker reads a copy
// committed here and has no runtime dependency on anyone.
//
//   node scripts/vendor-register.mjs 2026.10.1
//
// Writes src/register/searchable-names.json, every published name of every current
// entity; src/register/ancestors.json, each entity's groups and species, nearest first;
// and src/register/fold-cases.json, the register's test cases for its comparison
// rule (ADR-0019), which src/register/fold.test.ts holds fold() to.

import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";

const edition = process.argv[2];
if (!edition) {
  console.error(
    "usage: node scripts/vendor-register.mjs <edition, e.g. 2026.10.1>",
  );
  process.exit(2);
}

const url = `https://github.com/salish-sea/animals/releases/download/${edition}/register-tsv.tar.gz`;
const response = await fetch(url);
if (!response.ok) throw new Error(`${url}: ${response.status}`);
const dir = mkdtempSync(path.join(tmpdir(), "register-"));
const tarball = path.join(dir, "register.tar.gz");
writeFileSync(tarball, Buffer.from(await response.arrayBuffer()));
execFileSync("tar", ["-xzf", tarball, "-C", dir]);

const tsv = (name) => {
  const [found] = execFileSync("find", [dir, "-name", name])
    .toString()
    .trim()
    .split("\n");
  if (!found) throw new Error(`${name} is not in ${url}`);
  const [header, ...rows] = readFileSync(found, "utf8").trimEnd().split("\n");
  const columns = header.split("\t");
  return rows.map((row) =>
    Object.fromEntries(row.split("\t").map((value, i) => [columns[i], value])),
  );
};

const names = tsv("searchable_name.tsv")
  .filter((row) => row.retired !== "1")
  .map((row) => [
    row.entity_id,
    row.name,
    row.type,
    row.language,
    row.entity_label,
    row.entity_kind,
    row.entity_rank,
  ]);

const out = path.join(import.meta.dirname, "..", "src", "register");
writeFileSync(
  path.join(out, "searchable-names.json"),
  JSON.stringify({
    edition,
    columns: ["iri", "name", "type", "language", "label", "kind", "rank"],
    names,
  }) + "\n",
);
const ancestors = {};
for (const row of tsv("ancestor.tsv").sort((a, b) => a.depth - b.depth)) {
  (ancestors[row.entity_id] ??= []).push(row.ancestor_id);
}
writeFileSync(
  path.join(out, "ancestors.json"),
  JSON.stringify({ edition, ancestors }) + "\n",
);
writeFileSync(
  path.join(out, "fold-cases.json"),
  JSON.stringify(
    { edition, cases: tsv("fold_test.tsv").map((c) => [c.input, c.folded]) },
    null,
    2,
  ) + "\n",
);
console.log(`register ${edition}: ${names.length} names`);
