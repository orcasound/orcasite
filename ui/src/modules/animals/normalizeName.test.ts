import cases from "./normalize-name-cases.json";
import { normalizeName } from "./normalizeName";

// The register publishes these cases so that every program comparing its names can check
// that it compares them the same way
describe("normalizeName", () => {
  it.each(cases.cases)("normalizes %j to %j", (input, normalized) => {
    expect(normalizeName(input)).toBe(normalized);
  });
});
