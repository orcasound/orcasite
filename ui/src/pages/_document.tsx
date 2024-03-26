// Document needs to be customized in order to make MUI work with SSR
// https://mui.com/material-ui/integrations/nextjs/#pages-router
// https://github.com/mui/material-ui/blob/master/examples/material-ui-nextjs-pages-router-ts/pages/_document.tsx

import {
  documentGetInitialProps,
  DocumentHeadTags,
  DocumentHeadTagsProps,
} from "@mui/material-nextjs/v14-pagesRouter";
import {
  DocumentContext,
  DocumentProps,
  Head,
  Html,
  Main,
  NextScript,
} from "next/document";
import React from "react";

import theme from "@/styles/theme";

export default function MyDocument(
  props: DocumentProps & DocumentHeadTagsProps,
) {
  return (
    <Html lang="en">
      <Head>
        {/* PWA primary color */}
        <meta name="theme-color" content={theme.palette.primary.main} />
        {/*
            prefer svg favicon but use ico as fallback
            svg will change color based on prefers-color-scheme media query
            fallback ico is colorful to provide contrast on both light and dark
            `sizes` attribute is required for Chrome to pick svg over multi-size ico
            (see https://bugs.chromium.org/p/chromium/issues/detail?id=1450857 and
            https://bugs.chromium.org/p/chromium/issues/detail?id=1162276)
        */}
        <link rel="icon" href="/favicon.ico?v=2" sizes="48x48" />
        <link rel="icon" href="/favicon.svg?v=2" type="image/svg+xml" />
        <meta name="emotion-insertion-point" content="" />
        <DocumentHeadTags {...props} />
      </Head>
      <body>
        <Main />
        <NextScript />
      </body>
    </Html>
  );
}

MyDocument.getInitialProps = async (ctx: DocumentContext) => {
  const finalProps = await documentGetInitialProps(ctx);
  return finalProps;
};
