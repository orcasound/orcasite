import {
  Alert,
  Autocomplete,
  Avatar,
  Box,
  Button,
  Chip,
  Divider,
  Popover,
  TextField,
  Tooltip,
  Typography,
} from "@mui/material";
import _ from "lodash";
import { useMemo, useState } from "react";

import {
  Bout,
  BoutTagsQuery,
  useBoutTagsQuery,
  useCreateBoutTagMutation,
  useDeleteBoutTagMutation,
  useGetCurrentUserQuery,
  useSetBoutTagCertaintyMutation,
  useTagsQuery,
} from "@/graphql/generated";
import { describe, displayName, search } from "@/register";
import { fold } from "@/register/fold";

import {
  type AppliedTag,
  buttonsToOffer,
  chainAbove,
  deepest,
  isAbove,
  isTag,
  type TagKind,
  tagTree,
} from "./tagButtons";

type BoutTag = NonNullable<
  NonNullable<BoutTagsQuery["boutTags"]>["results"]
>[number];

/** What applying a tag sends: a button, a register name, an existing tag or free text */
type TagChoice = { name: string; kind?: TagKind | null; iri?: string | null };

type Option = TagChoice & { detail?: string };

/**
 * How sure the moderator is, one tap at a time. A tag is applied saying nothing about
 * it, which is what the column's nil means; the `?` steps it through probable,
 * possible and certain and back to nothing, so `certain` is something a moderator chose
 * rather than what every tap wrote. Hedging has to cost no more than not hedging (#1015).
 */
const certainties = [null, "probable", "possible", "certain"] as const;
const nextCertainty = (certainty: string | null) =>
  certainties[
    (certainties.indexOf((certainty ?? null) as never) + 1) % certainties.length
  ];
const certaintyWord: Record<string, string> = {
  probable: "probably",
  possible: "possibly",
  certain: "certainly",
};
const isHedge = (certainty: string | null | undefined) =>
  certainty === "probable" || certainty === "possible";

/** What a chip calls a tag: an animal by the register's name for it */
const tagLabel = (tag: AppliedTag) =>
  (tag.iri && displayName(tag.iri)) || tag.name;

const RECENT_KEY = "orcasite:recent-bout-tags";
const RECENT_LIMIT = 8;

function readRecent(): TagChoice[] {
  try {
    return JSON.parse(localStorage.getItem(RECENT_KEY) ?? "[]");
  } catch {
    return [];
  }
}

/**
 * The last tags this moderator applied, newest first, for the next bout. Only the one
 * they chose, not the chain above it, which comes with it again.
 */
function remember(choice: TagChoice): TagChoice[] {
  const recent = [
    choice,
    ...readRecent().filter((r) => !isTag(r, choice)),
  ].slice(0, RECENT_LIMIT);
  try {
    localStorage.setItem(RECENT_KEY, JSON.stringify(recent));
  } catch {
    // a private window: the row just doesn't persist
  }
  return recent;
}

export function BoutTags({ bout }: { bout: Pick<Bout, "id"> }) {
  const currentUser = useGetCurrentUserQuery().data?.currentUser;
  const moderator = currentUser?.moderator ?? false;
  const tagsQuery = useTagsQuery();
  const existingTags = useMemo(
    () => tagsQuery.data?.tags?.results ?? [],
    [tagsQuery.data],
  );
  const boutTagsQuery = useBoutTagsQuery({ boutId: bout.id });
  const boutTags: BoutTag[] = boutTagsQuery.data?.boutTags?.results ?? [];

  // by id: a username is optional, and a moderator without one owned nothing
  const mine = boutTags.filter(
    (bt) => currentUser?.id != null && bt.userId === currentUser.id,
  );
  const mineApplied: AppliedTag[] = mine.flatMap((bt) =>
    bt.tag ? [bt.tag] : [],
  );

  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [recent, setRecent] = useState<TagChoice[]>(readRecent);
  const [input, setInput] = useState("");
  const [tagAnchorEls, setTagAnchorEls] = useState<
    Record<string, HTMLDivElement | null>
  >({});

  const refetch = () => {
    boutTagsQuery.refetch();
    tagsQuery.refetch();
  };
  const createBoutTag = useCreateBoutTagMutation();
  const deleteBoutTag = useDeleteBoutTagMutation();
  const setCertainty = useSetBoutTagCertaintyMutation({
    onSuccess: refetch,
    onError: (e) => setError(String(e)),
  });

  /**
   * The name to send for a choice: an existing tag's own, when it is the same name by
   * the register's fold (`T036` and production's unclassified `T36`), so the server
   * finds that tag rather than making a second one for the same animal.
   */
  const nameFor = (choice: TagChoice) =>
    existingTags.find(
      (tag) =>
        fold(tag.name) === fold(choice.name) &&
        (!tag.iri || !choice.iri || tag.iri === choice.iri),
    )?.name ?? choice.name;

  /**
   * Apply a tag with everything above it that this moderator hasn't already applied,
   * top first, one application each and none of them hedged.
   */
  const applyChoice = async (choice: TagChoice) => {
    setError(null);
    setBusy(true);
    try {
      const chain: TagChoice[] = [
        ...chainAbove(choice).map((b) => ({
          name: b.name,
          kind: b.kind,
          iri: b.iri,
        })),
        choice,
      ].filter((c) => !mineApplied.some((tag) => isTag(c, tag)));
      for (const c of chain) {
        const data = await createBoutTag.mutateAsync({
          boutId: bout.id,
          tagName: nameFor(c),
          tagKind: c.kind ?? undefined,
          tagIri: c.iri ?? undefined,
        });
        const errors = data.createBoutTag?.errors ?? [];
        if (errors.length > 0) {
          setError(errors.map((e) => e?.message).join("; "));
          return;
        }
      }
      setRecent(remember(choice));
    } catch (e) {
      setError(String(e));
    } finally {
      setBusy(false);
      refetch();
    }
  };

  /** Remove this moderator's application of a tag, and of anything they put beneath it */
  const removeTag = async (tag: AppliedTag) => {
    setError(null);
    setBusy(true);
    try {
      for (const bt of mine) {
        if (bt.tag && (bt.tag === tag || isAbove(tag, bt.tag)))
          await deleteBoutTag.mutateAsync({ boutTagId: bt.id });
      }
    } catch (e) {
      setError(String(e));
    } finally {
      setBusy(false);
      refetch();
    }
  };

  // Typing reaches what the buttons don't: every name the register publishes, hidden
  // ones included (SRKW, T34s), then the tags moderators have already made
  const options: Option[] = useMemo(() => {
    const q = fold(input);
    if (!q) return [];
    const fromRegister: Option[] = search(input, 15).map((entity) => ({
      // as a button would name it, so the stored name doesn't depend on the path taken
      name: displayName(entity.iri) ?? entity.label,
      kind: "animal",
      iri: entity.iri,
      detail: describe(entity),
    }));
    const cited = new Set(fromRegister.map((o) => o.iri));
    const fromTags: Option[] = existingTags
      .filter((tag) => !(tag.iri && cited.has(tag.iri)))
      .filter((tag) => fold(tag.name).includes(q))
      .slice(0, 10)
      .map((tag) => ({
        name: tag.name,
        kind: tag.kind as TagKind | null,
        iri: tag.iri,
        detail: tag.kind ? `${tag.kind} tag` : "tag",
      }));
    return [...fromRegister, ...fromTags];
  }, [input, existingTags]);

  const recentToOffer = recent.filter(
    (r) => !mineApplied.some((tag) => isTag(r, tag)),
  );
  // the tags no other tag on the bout implies; the rest show in their popovers
  const groups = Object.values(_.groupBy(boutTags, (bt) => bt.tag?.slug));
  const shown = deepest(
    groups.flatMap((group) => (group[0].tag ? [group[0].tag] : [])),
  );

  return (
    <Box>
      {/* what is on the bout first, where a tap's result shows */}
      <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1, mb: 3 }}>
        {groups.map((group) => {
          const tag = group[0].tag;
          if (!tag || !shown.includes(tag)) return null;
          const tagSlug = tag.slug;
          const myTag = group.find((bt) => mine.includes(bt));
          const anchorEl = tagAnchorEls[tagSlug];
          const open = Boolean(anchorEl);
          const setAnchor = (el: HTMLDivElement | null) =>
            setTagAnchorEls((els) => ({ ...els, [tagSlug]: el }));
          const myWord = myTag?.certainty && certaintyWord[myTag.certainty];
          const implied = groups
            .map((g) => g[0].tag)
            .filter((t): t is NonNullable<typeof t> => !!t && isAbove(t, tag));

          return (
            <Box key={tagSlug} sx={{ display: "flex", alignItems: "center" }}>
              <Chip
                aria-describedby={open ? tagSlug : undefined}
                onClick={(event) => setAnchor(event.currentTarget)}
                variant={isHedge(myTag?.certainty) ? "outlined" : "filled"}
                sx={
                  isHedge(myTag?.certainty)
                    ? { borderStyle: "dashed" }
                    : undefined
                }
                {...(myTag && { onDelete: () => removeTag(tag) })}
                color={myTag ? "primary" : "default"}
                label={myWord ? `${tagLabel(tag)} (${myWord})` : tagLabel(tag)}
                icon={
                  group.length > 1 ? (
                    <Avatar sx={{ width: 24, height: 24, fontSize: 14 }}>
                      {group.length}
                    </Avatar>
                  ) : undefined
                }
              />
              {myTag && (
                <Tooltip title="How sure: probably → possibly → certainly → unsaid">
                  <Button
                    size="small"
                    sx={{ minWidth: 0, px: 1 }}
                    disabled={setCertainty.isPending}
                    aria-label={`How sure you are of ${tagLabel(tag)}`}
                    onClick={() =>
                      setCertainty.mutate({
                        boutTagId: myTag.id,
                        certainty: nextCertainty(myTag.certainty),
                      })
                    }
                  >
                    ?
                  </Button>
                </Tooltip>
              )}
              <Popover
                id={open ? tagSlug : undefined}
                open={open}
                anchorEl={anchorEl}
                onClose={() => setAnchor(null)}
                anchorOrigin={{ vertical: "bottom", horizontal: "left" }}
              >
                <Box
                  sx={{
                    p: 2,
                    display: "flex",
                    flexDirection: "column",
                    gap: 1,
                  }}
                >
                  <Typography variant="body1">{tagLabel(tag)}</Typography>
                  {tag.iri && (
                    <Typography variant="caption" color="text.secondary">
                      {tag.iri}
                      {tag.name !== tagLabel(tag) && ` · tagged as ${tag.name}`}
                    </Typography>
                  )}
                  {implied.length > 0 && (
                    <Typography variant="caption" color="text.secondary">
                      Also on the bout: {implied.map(tagLabel).join(", ")}
                    </Typography>
                  )}
                  {tag.description && (
                    <Typography variant="body2">{tag.description}</Typography>
                  )}
                </Box>
                <Divider />
                <Box sx={{ display: "flex", gap: 1, p: 2 }}>
                  {group.map((bt) => (
                    <Chip
                      key={bt.id}
                      label={`${bt.user?.username ?? "(no username)"}${
                        bt.certainty && certaintyWord[bt.certainty]
                          ? ` (${certaintyWord[bt.certainty]})`
                          : ""
                      }`}
                    />
                  ))}
                </Box>
              </Popover>
            </Box>
          );
        })}

        {boutTags.length === 0 && (
          <Typography sx={{ width: "100%", textAlign: "center" }}>
            No bout tags
          </Typography>
        )}
      </Box>
      {moderator && (
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
          {recentToOffer.length > 0 && (
            <ButtonRow label="Recent">
              {recentToOffer.map((choice) => (
                <Button
                  key={`${choice.iri ?? ""}:${choice.name}`}
                  size="small"
                  variant="outlined"
                  color="secondary"
                  disabled={busy}
                  onClick={() => applyChoice(choice)}
                >
                  {tagLabel(choice)}
                </Button>
              ))}
            </ButtonRow>
          )}
          {tagTree.map(({ section, buttons }) => (
            <ButtonRow key={section} label={section}>
              {/* against my own tags: another moderator's don't stop me seconding them */}
              {buttonsToOffer(buttons, mineApplied).map((button) => (
                <Button
                  key={button.iri ?? button.name}
                  size="small"
                  variant="outlined"
                  disabled={busy}
                  onClick={() =>
                    applyChoice({
                      name: button.name,
                      kind: button.kind,
                      iri: button.iri,
                    })
                  }
                >
                  {button.name}
                </Button>
              ))}
            </ButtonRow>
          ))}
          <Autocomplete<Option, false, false, true>
            freeSolo
            options={options}
            filterOptions={(o) => o}
            inputValue={input}
            onInputChange={(_event, value) => setInput(value)}
            value={null}
            onChange={(_event, value) => {
              if (!value) return;
              applyChoice(typeof value === "string" ? { name: value } : value);
              setInput("");
            }}
            getOptionLabel={(option) =>
              typeof option === "string" ? option : option.name
            }
            renderOption={(props, option) => {
              const { key, ...optionProps } = props;
              return (
                <li key={key} {...optionProps}>
                  <Box>
                    <Typography variant="body2">{option.name}</Typography>
                    {option.detail && (
                      <Typography variant="caption" color="text.secondary">
                        {option.detail}
                      </Typography>
                    )}
                  </Box>
                </li>
              );
            }}
            sx={{ width: 360 }}
            renderInput={(params) => (
              <TextField
                {...params}
                label="Find an animal or tag, or type a new one"
                variant="standard"
              />
            )}
          />
          {error && <Alert severity="error">{error}</Alert>}
        </Box>
      )}
    </Box>
  );
}

function ButtonRow({
  label,
  children,
}: {
  label: string;
  children: React.ReactNode;
}) {
  return (
    <Box sx={{ display: "flex", alignItems: "baseline", gap: 1 }}>
      <Typography
        variant="caption"
        color="text.secondary"
        sx={{ width: 64, flexShrink: 0 }}
      >
        {label}
      </Typography>
      <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1 }}>{children}</Box>
    </Box>
  );
}
