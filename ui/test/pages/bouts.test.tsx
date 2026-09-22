import { type BoutFixture, buildBout } from "@test/fixtures/bouts";
import {
  ASH_DEFAULT_MAX_PAGE_SIZE,
  offsetPage,
  stubGraphql,
} from "@test/graphql";
import { renderWithProviders, screen, waitFor, within } from "@test/utils";
import type { UserEvent } from "@testing-library/user-event";

import type { BoutsQueryVariables } from "@/graphql/generated";
import BoutsPage from "@/pages/bouts";

// Lives outside src/pages because Next turns every file in there into a route.

const DEFAULT_PAGE_SIZE = 50;
// $limit's default in the bouts query document, used when the page sends none
const QUERY_DEFAULT_LIMIT = 100;
const HOUR = 60 * 60_000;

/** `count` ended bouts an hour apart, newest first: "Bout 1" is the most recent. */
function buildEndedBouts(count: number): BoutFixture[] {
  const newest = new Date("2026-01-01T12:00:00Z").getTime();

  return Array.from({ length: count }, (_, index) =>
    buildBout(`bout_${index + 1}`, {
      name: `Bout ${index + 1}`,
      startTime: new Date(newest - index * HOUR),
    }),
  );
}

const isNewestFirst = (sort: BoutsQueryVariables["sort"]) =>
  [sort].flat().some((s) => s?.field === "START_TIME" && s.order === "DESC");

/**
 * The API as this page sees it: no bout in progress, no feeds, and `ended` served one
 * page at a time. A page at `holdOffset` never arrives; the first `failures`
 * requests for ended bouts are rejected, and the page at `failOffset` fails once.
 */
function serveBouts(
  ended: BoutFixture[],
  { holdOffset = -1, failures = 0, failOffset = -1 } = {},
) {
  let failuresLeft = failures;
  let failOnceAt = failOffset;

  stubGraphql({
    feeds: () => ({ feeds: [] }),
    detectionsCount: () => ({ feedDetectionsCount: 0 }),
    bouts: (variables: BoutsQueryVariables) => {
      const inProgress = variables.filter?.endTime?.isNil;
      const rows = inProgress
        ? []
        : isNewestFirst(variables.sort)
          ? ended
          : [...ended].reverse();
      const offset = variables.offset ?? 0;
      const limit = variables.limit ?? QUERY_DEFAULT_LIMIT;

      if (!inProgress && (failuresLeft > 0 || offset === failOnceAt)) {
        if (offset === failOnceAt) failOnceAt = -1;
        else failuresLeft--;
        throw new Error("bouts query failed");
      }
      if (!inProgress && offset === holdOffset) return new Promise(() => {});

      return { bouts: offsetPage(rows, { limit, offset }) };
    },
  });
}

// the list has a pagination control above it and one below; both drive the same page
const nextPageButton = () =>
  screen.getAllByRole("button", { name: /go to next page/i })[0];

const paginationControls = () =>
  screen.getAllByRole("combobox", { name: /rows per page/i });

const visibleBoutNames = () =>
  screen
    .queryAllByRole("heading", { level: 5, name: /^Bout \d+$/ })
    .map((heading) => heading.textContent);

async function chooseLargestPageSize(user: UserEvent) {
  await user.click(paginationControls()[0]);
  const sizes = within(screen.getByRole("listbox")).getAllByRole("option");
  const largest = sizes.reduce((a, b) =>
    Number(b.textContent) > Number(a.textContent) ? b : a,
  );
  await user.click(largest);
  await screen.findByRole("heading", {
    name: `Bout ${DEFAULT_PAGE_SIZE + 1}`,
  });
}

async function collectBoutsAcrossPages(user: UserEvent) {
  const seen = visibleBoutNames();

  while (!nextPageButton().hasAttribute("disabled")) {
    const firstOnPage = visibleBoutNames()[0];
    await user.click(nextPageButton());
    await waitFor(() => expect(visibleBoutNames()[0]).not.toBe(firstOnPage));
    seen.push(...visibleBoutNames());
  }

  return seen;
}

describe("BoutsPage", () => {
  it("keeps the current page on screen while the next one loads", async () => {
    serveBouts(buildEndedBouts(120), { holdOffset: DEFAULT_PAGE_SIZE });
    const { user } = renderWithProviders(<BoutsPage />);
    await screen.findByRole("heading", { name: "Bout 1" });

    await user.click(nextPageButton());

    expect(screen.getAllByText("51–100 of 120")[0]).toBeInTheDocument();
    expect(screen.getByRole("heading", { name: "Bout 1" })).toBeInTheDocument();
    expect(visibleBoutNames()).toHaveLength(DEFAULT_PAGE_SIZE);
  });

  it("reaches every ended bout, newest first, across pages at the largest page size", async () => {
    // more bouts than the server returns in one page: an option above the 250
    // cap would be clamped server side, skip rows, and fail this walk
    const bouts = buildEndedBouts(ASH_DEFAULT_MAX_PAGE_SIZE + 50);
    serveBouts(bouts);
    const { user } = renderWithProviders(<BoutsPage />);
    await screen.findByRole("heading", { name: "Bout 1" });
    await chooseLargestPageSize(user);

    const seen = await collectBoutsAcrossPages(user);

    expect(seen).toEqual(bouts.map((bout) => bout.name));
  });

  it("keeps the pagination controls and retries after a failed load", async () => {
    serveBouts(buildEndedBouts(60), { failures: 1 });
    const { user } = renderWithProviders(<BoutsPage />);

    await screen.findByText("The bouts list failed to load.");
    expect(paginationControls()).toHaveLength(2);

    await user.click(screen.getByRole("button", { name: /retry/i }));

    await screen.findByRole("heading", { name: "Bout 1" });
    expect(screen.getAllByText("1–50 of 60")[0]).toBeInTheDocument();
    expect(
      screen.queryByText("The bouts list failed to load."),
    ).not.toBeInTheDocument();
  });

  it("keeps the pagination controls when paging away from a failed page", async () => {
    serveBouts(buildEndedBouts(120), {
      failOffset: DEFAULT_PAGE_SIZE,
      holdOffset: 2 * DEFAULT_PAGE_SIZE,
    });
    const { user } = renderWithProviders(<BoutsPage />);
    await screen.findByRole("heading", { name: "Bout 1" });

    await user.click(nextPageButton());
    await screen.findByText("The bouts list failed to load.");
    expect(paginationControls()).toHaveLength(2);

    // the next page never settles; the errored page left no placeholder data,
    // so without the settled gate both controls would unmount here
    await user.click(nextPageButton());

    expect(paginationControls()).toHaveLength(2);
  });
});
