import { type BoutFixture, buildEndedBouts } from "@test/fixtures/bouts";
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

const isNewestFirst = (sort: BoutsQueryVariables["sort"]) =>
  [sort].flat().some((s) => s?.field === "START_TIME" && s.order === "DESC");

/**
 * The API as this page sees it: no bout in progress, no feeds, and `ended` served one
 * page at a time. A page at `holdOffset` never arrives.
 */
function serveBouts(ended: BoutFixture[], { holdOffset = -1 } = {}) {
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

      if (!inProgress && offset === holdOffset) return new Promise(() => {});

      return { bouts: offsetPage(rows, { limit, offset }) };
    },
  });
}

// the list has a pagination control above it and one below; both drive the same page
const nextPageButton = () =>
  screen.getAllByRole("button", { name: /go to next page/i })[0];

const visibleBoutNames = () =>
  screen
    .queryAllByRole("heading", { level: 5, name: /^Bout \d+$/ })
    .map((heading) => heading.textContent);

async function chooseLargestPageSize(user: UserEvent) {
  await user.click(
    screen.getAllByRole("combobox", { name: /rows per page/i })[0],
  );
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

  it("reaches every ended bout, newest first, at the largest page size", async () => {
    // more bouts than the server returns in one page, so an oversized page skips some
    const bouts = buildEndedBouts(ASH_DEFAULT_MAX_PAGE_SIZE + 50);
    serveBouts(bouts);
    const { user } = renderWithProviders(<BoutsPage />);
    await screen.findByRole("heading", { name: "Bout 1" });
    await chooseLargestPageSize(user);

    const seen = await collectBoutsAcrossPages(user);

    expect(seen).toEqual(bouts.map((bout) => bout.name));
  }, 30_000);
});
