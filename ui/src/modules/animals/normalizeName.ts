/**
 * The register's rule for comparing names (salish-sea/animals ADR-0019): two spellings
 * name the same entity when they normalize to the same string. Use it only to compare
 * names, never to display one: what is shown keeps its capitals, zero-padding and
 * apostrophes.
 *
 * Exactly four steps, in order. A trailing `s` is kept, because `T090s` is the
 * matriline and `T090` its matriarch.
 */
export function normalizeName(name: string): string {
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
