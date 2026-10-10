import { fold } from "./fold";
import cases from "./fold-cases.json";

// The register publishes these cases so that every consumer's fold can be held to its own
describe("fold", () => {
  it.each(cases.cases)("folds %j to %j", (input, folded) => {
    expect(fold(input)).toBe(folded);
  });
});
