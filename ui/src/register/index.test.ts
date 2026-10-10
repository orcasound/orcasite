import { displayName, search } from ".";

describe("search", () => {
  it("finds an entity by a name it is never shown by", () => {
    expect(search("SRKW")[0]).toMatchObject({
      iri: "SSA:0000010",
      label: "Southern Resident",
    });
  });

  it("forgives padding, hyphens and apostrophes, as the register's fold does", () => {
    expect(search("T34s")[0].iri).toBe("SSA:0002014");
    expect(search("Biggs")[0].iri).toBe("SSA:0000002");
    expect(search("J-35")[0].iri).toBe("SSA:0000101");
  });

  it("offers both entities a bare designation names, the family and the animal", () => {
    const exact = search("T37").slice(0, 2);
    expect(exact.map((e) => e.rank || e.kind).sort()).toEqual([
      "individual",
      "matriline",
    ]);
  });

  it("finds nothing for nothing", () => {
    expect(search("  ")).toEqual([]);
  });
});

describe("displayName", () => {
  it("calls a species by its common name and a group by its label", () => {
    expect(displayName("SSA:0000900")).toBe("Killer whale");
    expect(displayName("SSA:0000020")).toBe("J pod");
  });

  it("uses an English common name, not the first one listed", () => {
    expect(displayName("SSA:0000901")).toBe("Humpback whale");
  });
});
