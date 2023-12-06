import { Box, Container, Paper } from "@mui/material";
import Image from "next/image";
import { ReactElement } from "react";

import Header from "@/components/Header";
import logo from "@/public/wordmark/wordmark-teal.svg";

function AuthLayout({ children }: { children: React.ReactNode }) {
  return (
    <Box
      sx={{
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
                marginLeft: "auto",
                marginRight: "auto",
                position: "relative",
                top: 0,
                minHeight: 100,
                maxWidth: { xs: "100%", sm: 350 },
              }}
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
