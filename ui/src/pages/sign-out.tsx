import { Box, Paper, Typography } from "@mui/material";
import Head from "next/head";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { useEffect } from "react";

import Header from "@/components/Header";
import { useSignOutMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import logo from "@/public/wordmark/wordmark-teal.svg";

const SignInPage: NextPageWithLayout = () => {
  const router = useRouter();
  const signOut = useSignOutMutation({
    onSuccess: () => {
      router.push("/");
    },
  });

  useEffect(() => {
    signOut.mutate({});
  }, []);

  return (
    <div>
      <Head>
        <title>Sign out | Orcasound</title>
      </Head>

      <main>
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
            <Paper
              sx={{ p: 4, maxWidth: 500, m: 4, minWidth: { sm: 250 } }}
              elevation={0}
            >
              <Box
                display="flex"
                justifyContent="center"
                alignItems="center"
                flexDirection={"column"}
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

              <Box display={"block"}>
                <Typography variant="body1" textAlign="center">
                  Signing out...
                </Typography>
              </Box>
            </Paper>
          </Box>
        </Box>
      </main>
    </div>
  );
};

export default SignInPage;
