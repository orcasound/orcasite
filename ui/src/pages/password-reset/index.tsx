import { Box, Paper } from "@mui/material";
import Head from "next/head";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { useState } from "react";

import ResetPasswordRequestForm from "@/components/Auth/ResetPasswordRequestForm";
import Header from "@/components/Header";
import { useRequestPasswordResetMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import logo from "@/public/wordmark/wordmark-teal.svg";

const RegisterPage: NextPageWithLayout = () => {
  const router = useRouter();

  const [message, setMessage] = useState<string>();

  const submitPasswordResetRequest = useRequestPasswordResetMutation({
    onMutate: () => {
      setMessage(undefined);
    },
    onSuccess: ({ requestPasswordReset }) => {
      if (requestPasswordReset) {
        setMessage("Password reset has been sent to the email provided.");
      }
    },
    onError: (error) => {
      console.log("Forgot password error", error);
    },
  });

  return (
    <div>
      <Head>
        <title>Register | Orcasound</title>
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
              sx={{
                p: 4,
                maxWidth: { xs: "100%", sm: 500 },
                minWidth: { sm: 500 },
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
              <ResetPasswordRequestForm
                onSubmit={(email) =>
                  submitPasswordResetRequest.mutate({
                    email,
                  })
                }
                message={message}
              />
            </Paper>
          </Box>
        </Box>
      </main>
    </div>
  );
};

export default RegisterPage;
