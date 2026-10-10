/**
 * The register's comparison rule for names (salish-sea/animals ADR-0019): two spellings
 * name the same entity when they fold to the same string. A comparison, never a
 * rewrite: what is displayed keeps its capitals, zero-padding and apostrophes.
 *
 * Exactly four steps, in order. A trailing `s` never folds, because `T090s` is the
 * matriline and `T090` its matriarch.
 */
export function fold(name: string): string {
  return (
    name
      .toLowerCase()
      .replace(/['’-]/g, "")
      .replace(/\s+/g, " ")
      .trim()
      // as text, not as a number, so a long run keeps every digit
      .replace(/\d+/g, (digits) => digits.replace(/^0+(?=\d)/, ""))
  );
}
