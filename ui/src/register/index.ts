import ancestry from "./ancestors.json";
import { fold } from "./fold";
import register from "./searchable-names.json";

/**
 * The salish-sea/animals register as the tag picker needs it (#1015): which entity a
 * typed name means, and what to call an entity on a button. From the edition copied in by
 * scripts/vendor-register.mjs.
 */

type Row = [
  iri: string,
  name: string,
  type: string,
  language: string,
  label: string,
  kind: string,
  rank: string,
];

type Entity = {
  iri: string;
  /** The register's label for it, which is what a tag created from it is called */
  label: string;
  /** `taxon`, `group` or `individual` */
  kind: string;
  /** For a group: `ecotype`, `community`, `clan`, `pod`, `matriline` */
  rank: string;
};

const rows = register.names as Row[];
const folded = rows.map((row) => fold(row[1]));

const entities = new Map<string, Entity>();
const commonNames = new Map<string, string>();
for (const [iri, name, type, language, label, kind, rank] of rows) {
  if (!entities.has(iri)) entities.set(iri, { iri, label, kind, rank });
  if (type === "common" && language === "en" && !commonNames.has(iri))
    commonNames.set(iri, name);
}

/**
 * What a button or a chip calls an entity: a species or other taxon by its English
 * common name (Killer whale, not Orcinus orca), anything else by the register's label
 * (J pod, Bigg's). Hidden names are for matching, and never shown.
 */
export function displayName(iri: string): string | undefined {
  const found = entities.get(iri);
  if (!found) return undefined;
  return (found.kind === "taxon" && commonNames.get(iri)) || found.label;
}

/** What kind of thing an entity is, in words a moderator uses */
export function describe(found: Entity): string {
  if (found.kind === "taxon") return "species or group of species";
  return found.rank || found.kind;
}

/**
 * The entities a typed name could mean, best first: every name that folds to exactly
 * what was typed (bare `T37` is both a matriline and an animal, and both are offered),
 * then names that begin with it, then names with a word that does. One entry per
 * entity, however many of its names match.
 */
export function search(query: string, limit = 20): Entity[] {
  const q = fold(query);
  if (!q) return [];
  const tiers: Entity[][] = [[], [], []];
  const seen = new Set<string>();
  rows.forEach((row, i) => {
    const name = folded[i];
    const tier =
      name === q ? 0 : name.startsWith(q) ? 1 : name.includes(` ${q}`) ? 2 : -1;
    if (tier < 0) return;
    tiers[tier].push(entities.get(row[0])!);
  });
  const results: Entity[] = [];
  for (const found of tiers.flat()) {
    if (seen.has(found.iri)) continue;
    seen.add(found.iri);
    results.push(found);
    if (results.length === limit) break;
  }
  return results;
}

const ancestors = ancestry.ancestors as Record<string, string[]>;

/**
 * The groups and species an entity belongs to, nearest first: T034s is in Bigg's, which
 * is Orcinus orca. Not the taxa above a species, which the register leaves to its
 * taxonomy.
 */
export function ancestorsOf(iri: string): string[] {
  return ancestors[iri] ?? [];
}
