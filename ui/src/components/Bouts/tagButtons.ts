import { ancestorsOf as registerAncestorsOf, displayName } from "@/register";

/**
 * The buttons a moderator tags a bout with (#1015), after the OrcaHello moderator
 * portal's (orcasound/orcahello#561): a short hand-picked tree, where applying a tag
 * opens its children. Tagging only as deep as you are sure is how a moderator says "an
 * orca, I can't tell which".
 *
 * A tag is applied with everything above it, as OrcaHello stores `J pod;srkw;orca;whale`
 * and as moderators have tagged bouts by hand (every J, K or L bout also carries SRKW):
 * J pod brings Southern Resident, Killer whale and Cetacean. Each level is its own
 * application with its own certainty, so "certainly Southern Resident, possibly L pod"
 * can be said. Only the deepest tags are shown; the rest are implied.
 *
 * An animal is a register entity, named as the register names it; the tree may skip
 * the register's levels (Southern Resident sits directly under Killer whale, without
 * the Resident ecotype) to save a tap. Anything deeper than a pod, every Bigg's
 * matriline and animal among them, is found by typing. Sounds and everything else are
 * tags by name, the names production's moderators already use.
 */

export type TagKind = "animal" | "signal" | "other";

export type TagButton = {
  name: string;
  kind: TagKind;
  /** The register entity an animal button applies */
  iri?: string;
  children?: TagButton[];
};

const animal = (iri: string, children?: TagButton[]): TagButton => ({
  name: displayName(iri) ?? iri,
  kind: "animal",
  iri,
  children,
});

const tag =
  (kind: TagKind) =>
  (name: string, children?: TagButton[]): TagButton => ({
    name,
    kind,
    children,
  });
const signal = tag("signal");
const other = tag("other");

export const tagTree: { section: string; buttons: TagButton[] }[] = [
  {
    section: "Animals",
    buttons: [
      animal("SSA:0000934", [
        animal("SSA:0000900", [
          animal("SSA:0000010", [
            animal("SSA:0000020"),
            animal("SSA:0000021"),
            animal("SSA:0000022"),
          ]),
          animal("SSA:0000002"),
        ]),
        animal("SSA:0000901"),
        animal("SSA:0000905"),
        animal("SSA:0000912"),
      ]),
      animal("SSA:0000938", [
        animal("SSA:0000903"),
        animal("SSA:0000902"),
        animal("SSA:0000904"),
      ]),
      animal("SSA:0000907", [animal("SSA:0000908"), animal("SSA:0000909")]),
    ],
  },
  {
    section: "Sounds",
    buttons: [
      signal("call"),
      signal("whistle"),
      signal("buzz"),
      signal("percussive"),
    ],
  },
  {
    section: "Other",
    buttons: [
      other("vessel", [
        other("ferry"),
        other("tug"),
        other("container"),
        other("bulk-carrier"),
        other("noncommercial-small"),
      ]),
      other("train"),
      other("airplane"),
      other("piledriving"),
      other("water"),
      other("60Hz-hum"),
      other("mystery"),
    ],
  },
];

/** A tag already on the bout, as far as the buttons care */
export type AppliedTag = { name: string; iri?: string | null };

/**
 * Whether a button is that tag: the same register entity, or the same name compared as
 * the server's unique index compares names, ignoring case. Production's `J` is the J
 * pod button, because it cites the same entity.
 */
export function isTag(button: AppliedTag, tag: AppliedTag): boolean {
  if (button.iri && tag.iri) return button.iri === tag.iri;
  return button.name.toLowerCase() === tag.name.toLowerCase();
}

const appliedIn = (button: TagButton, applied: AppliedTag[]) =>
  applied.some((tag) => isTag(button, tag));

/** Whether this button, or any button beneath it, is on the bout */
function isOpen(button: TagButton, applied: AppliedTag[]): boolean {
  return (
    appliedIn(button, applied) ||
    (button.children ?? []).some((child) => isOpen(child, applied))
  );
}

/**
 * The buttons to offer: every top-level button, and the children of every tag on the
 * bout and of everything above it, less what is already on the bout or above it.
 * Having J pod open keeps K and L pod and Bigg's on offer beside it.
 */
export function buttonsToOffer(
  buttons: TagButton[],
  applied: AppliedTag[],
): TagButton[] {
  return buttons.flatMap((button) =>
    isOpen(button, applied)
      ? buttonsToOffer(button.children ?? [], applied)
      : [button],
  );
}

const sections = tagTree.flatMap(({ buttons }) => buttons);

/** The buttons from the top of the tree down to the one that is this tag, or none */
function treePath(
  buttons: TagButton[],
  tag: AppliedTag,
): TagButton[] | undefined {
  for (const button of buttons) {
    if (isTag(button, tag)) return [button];
    const below = treePath(button.children ?? [], tag);
    if (below) return [button, ...below];
  }
  return undefined;
}

/**
 * The tree's buttons above a tag, top first: what applying it applies too. A tag the
 * tree doesn't have, such as a matriline found by typing, hangs from the nearest of its
 * register groups that the tree does have: T034s is applied with Bigg's, Killer whale
 * and Cetacean.
 */
export function chainAbove(tag: AppliedTag): TagButton[] {
  const path = treePath(sections, tag);
  if (path) return path.slice(0, -1);
  for (const iri of tag.iri ? registerAncestorsOf(tag.iri) : []) {
    const above = treePath(sections, { name: "", iri });
    if (above) return above;
  }
  return [];
}

/** Whether `above` is implied by `tag`: in the tree's chain above it, or a register group of it */
export function isAbove(above: AppliedTag, tag: AppliedTag): boolean {
  if (chainAbove(tag).some((button) => isTag(button, above))) return true;
  return (
    !!tag.iri && !!above.iri && registerAncestorsOf(tag.iri).includes(above.iri)
  );
}

/** The tags no other tag on the bout implies: the ones to show */
export function deepest<T extends AppliedTag>(applied: T[]): T[] {
  return applied.filter(
    (tag) => !applied.some((other) => other !== tag && isAbove(tag, other)),
  );
}
