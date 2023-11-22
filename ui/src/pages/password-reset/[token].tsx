import { Box, Paper } from "@mui/material";
import Head from "next/head";
import Image from "next/image";
import { useRouter } from "next/router";
import { useState } from "react";

import ResetPasswordForm from "@/components/Auth/ResetPasswordForm";
import Header from "@/components/Header";
import { MutationError, useResetPasswordMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import logo from "@/public/wordmark/wordmark-teal.svg";
import { setAuthToken, setCurrentUser } from "@/utils/auth";

const PasswordResetPage: NextPageWithLayout = () => {
  const router = useRouter();
  const token = router.query.token as string;

  const [errors, setErrors] = useState<MutationError[]>([]);

  const submitResetPassword = useResetPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ resetPassword }) => {
      if (resetPassword) {
        const { token, errors, user } = resetPassword;

        if (errors && errors?.length > 0) {
          setErrors(
            errors.filter((error): error is MutationError => error !== null),
          );
        }

        if (token) {
          setAuthToken(token);
        }

        if (user) {
          setCurrentUser(user);
          router.push("/");
        }
      }
    },
    onError: (error) => {
      console.log("Forgot password error", error);
    },
  });

  return (
    <div>
      <Head>
        <title>Reset password | Orcasound</title>
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
              <ResetPasswordForm
                onSubmit={(password, passwordConfirmation) =>
                  submitResetPassword.mutate({
                    password,
                    passwordConfirmation,
                    resetToken: token,
                  })
                }
                errors={errors}
              />
            </Paper>
          </Box>
        </Box>
      </main>
    </div>
  );
};

export default PasswordResetPage;
