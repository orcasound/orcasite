import { Box, Container, Paper } from "@mui/material";
import Image from "next/image";
import { ReactElement } from "react";

import Header from "@/components/Header";
import logo from "@/public/wordmark/wordmark-teal.svg";

function AuthLayout({ children }: { children: React.ReactNode }) {
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
      <Box
        display="flex"
        justifyContent="center"
        alignItems="center"
        sx={{ flexGrow: 1 }}
      >
        <Container maxWidth="sm">
          <Paper
            sx={{
              p: 4,
              m: 4,
            }}
          >
            <Box
              display="flex"
              justifyContent="center"
              alignItems="center"
              sx={{
                marginBottom: 4,
                overflow: "hidden",
                marginLeft: "auto",
                marginRight: "auto",
              }}
              position="relative"
              top={0}
              minHeight={100}
              maxWidth={{ xs: "100%", sm: 350 }}
            >
              <Image src={logo} alt="Orcasound" fill />
            </Box>
            {children}
          </Paper>
        </Container>
      </Box>
    </Box>
  );
}

export function getAuthLayout(page: ReactElement) {
  return <AuthLayout>{page}</AuthLayout>;
}
