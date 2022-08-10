import { CacheProvider, EmotionCache } from '@emotion/react'
import CssBaseline from '@mui/material/CssBaseline'
import { ThemeProvider } from '@mui/material/styles'
import {
  Hydrate,
  QueryClient,
  QueryClientProvider,
} from '@tanstack/react-query'
import { NextPage } from 'next'
import type { AppProps } from 'next/app'
import Head from 'next/head'
import { ReactElement, ReactNode, useState } from 'react'

import createEmotionCache from '../styles/createEmotionCache'
import theme from '../styles/theme'

export type NextPageWithLayout<P = {}, IP = P> = NextPage<P, IP> & {
  getLayout?: (page: ReactElement) => ReactNode
}

type MyAppProps = AppProps & {
  Component: NextPageWithLayout
  emotionCache?: EmotionCache
}

// Client-side cache, shared for the whole session of the user in the browser.
// https://github.com/mui/material-ui/blob/master/examples/nextjs-with-typescript/pages/_app.tsx
const clientSideEmotionCache = createEmotionCache()

function MyApp({
  Component,
  pageProps,
  emotionCache = clientSideEmotionCache,
}: MyAppProps) {
  // Allow pages to define custom per-page layout
  // Based on https://nextjs.org/docs/basic-features/layouts
  const getLayout = Component.getLayout || ((page) => page)

  // Configure react-query using the hydration setup
  // https://react-query.tanstack.com/guides/ssr#using-hydration
  const [queryClient] = useState(() => new QueryClient())

  return (
    <QueryClientProvider client={queryClient}>
      <Hydrate state={pageProps.dehydratedState}>
        <CacheProvider value={emotionCache}>
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
        </CacheProvider>
      </Hydrate>
    </QueryClientProvider>
  )
}
export default MyApp
