import { Box, Container } from "@mui/material";
import { ReactElement } from "react";

import Header from "@/components/Header";

function ReportsLayout({ children }: { children: React.ReactNode }) {
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
      <Header />
      <Box sx={{ flexGrow: 1, display: "flex" }}>
        <Container maxWidth="xl">{children}</Container>
      </Box>
    </Box>
  );
}

export function getReportsLayout(page: ReactElement) {
  return <ReportsLayout>{page}</ReportsLayout>;
}
