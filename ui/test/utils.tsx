import { ThemeProvider } from "@mui/material/styles";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { render, type RenderOptions } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { RouterContext } from "next/dist/shared/lib/router-context.shared-runtime";
import type { NextRouter } from "next/router";
import type { ReactElement, ReactNode } from "react";

import theme from "@/styles/theme";

export * from "@testing-library/react";

export function createMockRouter(
  overrides: Partial<NextRouter> = {},
): NextRouter {
  return {
    basePath: "",
    pathname: "/",
    route: "/",
    asPath: "/",
    query: {},
    isReady: true,
    isPreview: false,
    isLocaleDomain: false,
    isFallback: false,
    push: vi.fn().mockResolvedValue(true),
    replace: vi.fn().mockResolvedValue(true),
    reload: vi.fn(),
    back: vi.fn(),
    forward: vi.fn(),
    prefetch: vi.fn().mockResolvedValue(undefined),
    beforePopState: vi.fn(),
    events: { on: vi.fn(), off: vi.fn(), emit: vi.fn() },
    ...overrides,
  } as NextRouter;
}

// retry: false matters. React Query retries three times with exponential backoff by
// default, which turns a test asserting that a request failed into a slow timeout.
export function createTestQueryClient() {
  return new QueryClient({
    defaultOptions: {
      queries: { retry: false, staleTime: 0, refetchOnWindowFocus: false },
      mutations: { retry: false },
    },
  });
}

type ProviderOptions = {
  queryClient?: QueryClient;
  router?: Partial<NextRouter>;
};

/**
 * Renders with the providers from _app.tsx that components need. Use this instead of
 * Testing Library's `render`, or components will throw:
 *
 * - without the app theme, `sx` callbacks reading its accent2/accent4 palette entries
 *   fail with "Cannot read properties of undefined (reading 'main')", because MUI's
 *   default theme doesn't define them
 * - without a router, anything containing @/components/Link throws "NextRouter was not
 *   mounted", since that component always calls useRouter()
 * - without a QueryClient, any component calling a generated GraphQL hook throws
 *   "No QueryClient set"
 */
export function renderWithProviders(
  ui: ReactElement,
  {
    queryClient,
    router,
    ...options
  }: ProviderOptions & Omit<RenderOptions, "wrapper"> = {},
) {
  const client = queryClient ?? createTestQueryClient();
  const mockRouter = createMockRouter(router);
  const user = userEvent.setup();

  const Wrapper = ({ children }: { children: ReactNode }) => (
    <RouterContext.Provider value={mockRouter}>
      <QueryClientProvider client={client}>
        <ThemeProvider theme={theme}>{children}</ThemeProvider>
      </QueryClientProvider>
    </RouterContext.Provider>
  );

  return {
    ...render(ui, { wrapper: Wrapper, ...options }),
    user,
    queryClient: client,
    router: mockRouter,
  };
}
