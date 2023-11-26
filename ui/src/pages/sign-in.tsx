import { Box, Paper } from "@mui/material";
import Head from "next/head";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { useState } from "react";

import SignInForm from "@/components/Auth/SignInForm";
import Header from "@/components/Header";
import { useSignInWithPasswordMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import logo from "@/public/wordmark/wordmark-teal.svg";
import { setAuthToken, setCurrentUser } from "@/utils/auth";

const SignInPage: NextPageWithLayout = () => {
  const router = useRouter();

  const [errors, setErrors] = useState<string[]>([]);

  const submitSignIn = useSignInWithPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ signInWithPassword }) => {
      if (signInWithPassword) {
        const { token, user, errors } = signInWithPassword;

        if (errors && errors?.length > 0) {
          setErrors(
            errors.flatMap((error) => {
              const message = error?.message || error?.code;
              if (typeof message === "string") {
                return [message];
              }
              return [];
            }),
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
    onError: (error: Error) => {
      setErrors([error.message]);
    },
  });

  return (
    <div>
      <Head>
        <title>Sign in | Orcasound</title>
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
            <Paper sx={{ p: 4, maxWidth: 500, m: 4 }}>
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
              <SignInForm
                onSubmit={(email, password) =>
                  submitSignIn.mutate({ email, password })
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

export default SignInPage;
