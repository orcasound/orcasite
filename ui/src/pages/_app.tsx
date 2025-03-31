import CssBaseline from "@mui/material/CssBaseline";
import { ThemeProvider } from "@mui/material/styles";
import { AppCacheProvider } from "@mui/material-nextjs/v14-pagesRouter";
import {
  HydrationBoundary,
  QueryClient,
  QueryClientProvider,
} from "@tanstack/react-query";
import { ReactQueryDevtools } from "@tanstack/react-query-devtools";
import { NextPage } from "next";
import type { AppProps } from "next/app";
import Head from "next/head";
import { Socket } from "phoenix";
import {
  createContext,
  ReactElement,
  ReactNode,
  SetStateAction,
  useState,
} from "react";
import React from "react";
import ReactGA from "react-ga4";

import theme from "@/styles/theme";
import { GA_TRACKING_ID } from "@/utils/analytics";

export type NextPageWithLayout<P = object, IP = P> = NextPage<P, IP> & {
  getLayout?: (page: ReactElement) => ReactNode;
};

type AppPropsWithLayout = AppProps & {
  Component: NextPageWithLayout;
};

if (GA_TRACKING_ID) {
  ReactGA.initialize(GA_TRACKING_ID);
}

// From https://tanstack.com/query/latest/docs/framework/react/devtools#modern-bundlers
const ReactQueryDevtoolsProd = React.lazy(() =>
  import("@tanstack/react-query-devtools/production").then((d) => ({
    default: d.ReactQueryDevtools,
  })),
);

export const SocketContext = createContext<{
  socket?: Socket;
  setSocket?: React.Dispatch<SetStateAction<Socket | undefined>>;
}>({});

// App needs to be customized in order to make MUI work with SSR
// https://mui.com/material-ui/integrations/nextjs/#pages-router
// https://github.com/mui/material-ui/blob/master/examples/material-ui-nextjs-pages-router-ts/pages/_app.tsx
export default function OrcasiteApp(props: AppPropsWithLayout) {
  const { Component, pageProps } = props;
  const [socket, setSocket] = useState<Socket>();

  // Allow pages to define custom per-page layout
  // Based on https://nextjs.org/docs/pages/building-your-application/routing/pages-and-layouts#with-typescript
  const getLayout = Component.getLayout ?? ((page) => page);

  // Configure react-query using the hydration setup
  // https://tanstack.com/query/latest/docs/framework/react/guides/ssr
  const [queryClient] = useState(
    () =>
      new QueryClient({
        defaultOptions: {
          queries: {
            staleTime: 1000 * 20, // 20 seconds
          },
        },
      }),
  );

  // From https://tanstack.com/query/latest/docs/framework/react/devtools#devtools-in-production
  const [showReactQueryDevtoolsProd, setShowReactQueryDevtoolsProd] =
    React.useState(false);

  React.useEffect(() => {
    // @ts-expect-error toggleQueryDevtools doesn't exist on window
    window.toggleQueryDevtools = () =>
      setShowReactQueryDevtoolsProd((old) => !old);
  }, []);

  return (
    <QueryClientProvider client={queryClient}>
      <HydrationBoundary state={pageProps.dehydratedState}>
        <SocketContext.Provider value={{ socket, setSocket }}>
          <AppCacheProvider {...props}>
            <Head>
              <title>Orcasound</title>
              <meta
                name="viewport"
                content="initial-scale=1, width=device-width"
              />
            </Head>
            <ThemeProvider theme={theme}>
              <CssBaseline />
              {getLayout(<Component {...pageProps} />)}
            </ThemeProvider>
          </AppCacheProvider>
        </SocketContext.Provider>
      </HydrationBoundary>
      {showReactQueryDevtoolsProd && (
        <React.Suspense fallback={null}>
          <ReactQueryDevtoolsProd />
        </React.Suspense>
      )}
      <ReactQueryDevtools initialIsOpen={false} />
    </QueryClientProvider>
  );
}
