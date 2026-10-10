import {
  type AppliedTag,
  buttonsToOffer,
  chainAbove,
  deepest,
  type TagButton,
  tagTree,
} from "./tagButtons";

const animals = tagTree[0].buttons;
const names = (buttons: TagButton[]) => buttons.map((b) => b.name);

describe("buttonsToOffer", () => {
  it("offers the top of the tree on an untagged bout", () => {
    expect(names(buttonsToOffer(animals, []))).toEqual([
      "Cetacean",
      "Pinniped",
      "Bird",
    ]);
  });

  it("opens a tag's children once it is applied", () => {
    expect(
      names(
        buttonsToOffer(animals, [{ name: "Cetacean", iri: "SSA:0000934" }]),
      ),
    ).toEqual([
      "Killer whale",
      "Humpback whale",
      "Gray whale",
      "Harbour porpoise",
      "Pinniped",
      "Bird",
    ]);
  });

  it("keeps a pod's siblings on offer, so J, K and L can all be applied", () => {
    // production's J, which cites J pod under a name of its own
    const applied: AppliedTag[] = [{ name: "J", iri: "SSA:0000020" }];
    expect(names(buttonsToOffer(animals, applied))).toEqual([
      "K pod",
      "L pod",
      "Bigg's",
      "Humpback whale",
      "Gray whale",
      "Harbour porpoise",
      "Pinniped",
      "Bird",
    ]);
  });

  it("matches a tag with no identifier by name, ignoring case", () => {
    expect(
      names(buttonsToOffer(tagTree[2].buttons, [{ name: "Vessel" }])),
    ).toContain("ferry");
  });
});

describe("chainAbove", () => {
  it("is every button above a tag in the tree, top first", () => {
    expect(names(chainAbove({ name: "J pod", iri: "SSA:0000020" }))).toEqual([
      "Cetacean",
      "Killer whale",
      "Southern Resident",
    ]);
  });

  it("finds a tag the tree doesn't have by its nearest register group that it does", () => {
    // T034s, found by typing, is a Bigg's matriline
    expect(names(chainAbove({ name: "T34s", iri: "SSA:0002014" }))).toEqual([
      "Cetacean",
      "Killer whale",
      "Bigg's",
    ]);
  });

  it("is nothing for a tag at the top, or one nobody has placed", () => {
    expect(chainAbove({ name: "Cetacean", iri: "SSA:0000934" })).toEqual([]);
    expect(chainAbove({ name: "mystery whup" })).toEqual([]);
  });
});

describe("deepest", () => {
  it("shows the pods and implies the rest, under production's own names", () => {
    const applied: AppliedTag[] = [
      { name: "KW", iri: "SSA:0000900" },
      { name: "SRKW", iri: "SSA:0000010" },
      { name: "J", iri: "SSA:0000020" },
      { name: "K", iri: "SSA:0000021" },
      { name: "call" },
    ];
    expect(deepest(applied).map((t) => t.name)).toEqual(["J", "K", "call"]);
  });

  it("uses the register's groups for tags the tree doesn't have", () => {
    const applied: AppliedTag[] = [
      { name: "Bigg's", iri: "SSA:0000002" },
      { name: "T37", iri: "SSA:0010082" },
    ];
    expect(deepest(applied).map((t) => t.name)).toEqual(["T37"]);
  });
});
