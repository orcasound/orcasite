import { Box, CssBaseline } from "@mui/material";
import { ThemeProvider } from "@mui/material";
import * as React from "react";
import { useEffect, useState } from "react";

import { DataProvider } from "@/context/DataContext";
import { NowPlayingProvider } from "@/context/NowPlayingContext";
import { useMasterData } from "@/hooks/useMasterData";
import darkTheme from "@/styles/darkTheme";

export function MasterDataLayout({ children }: { children: React.ReactNode }) {
  // use toggle switch in dev mode between live API data vs seed data
  const [useLiveData, setUseLiveData] = useState(true);
  const dataset = useMasterData(useLiveData);

  // testing
  useEffect(() => {
    console.log("human.length: " + dataset.human.length);
    console.log("ai.length: " + dataset.ai.length);
    console.log("sightings.length: " + dataset.sightings.length);
    console.log("combined.length " + dataset.combined.length);
    console.log("feeds.length: " + dataset.feeds.length);
    console.log("isSuccessOrcahello.length: " + dataset.isSuccessOrcahello);
  }, [dataset]);

  //// RENDER

  return (
    <Box
      sx={{
        // use `dvh` for dynamic viewport height to handle mobile browser weirdness
        // but fallback to `vh` for browsers that don't support `dvh`
        // `&` is a workaround because sx prop can't have identical keys
        "&": {
          height: "100dvh",
        },
        height: "100vh",
        display: "flex",
        flexDirection: "column",
      }}
    >
      {process.env.NODE_ENV === "development" && (
        <button
          onClick={() => setUseLiveData((prev) => !prev)}
          style={{
            position: "fixed",
            zIndex: 10000,
            bottom: "4%",
            right: "5%",
            background: "yellow",
          }}
        >
          {useLiveData ? "Using LIVE data" : "Using SEED data"}
        </button>
      )}
      <NowPlayingProvider>
        <ThemeProvider theme={darkTheme}>
          <CssBaseline />
          <DataProvider data={dataset}>{children}</DataProvider>
        </ThemeProvider>
      </NowPlayingProvider>
    </Box>
  );
}

// Not using this currently, removing to satisfy ES Lint
// export function getMasterDataLayout(page: ReactElement) {
//   return <MasterDataLayout>{page}</MasterDataLayout>;
// }
