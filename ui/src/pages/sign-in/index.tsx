import { Box, Paper } from "@mui/material";
import Head from "next/head";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { useState } from "react";

import SigninForm from "@/components/Auth/SigninForm";
import Header from "@/components/Header";
import {
  MutationError,
  useSignInWithPasswordMutation,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import logo from "@/public/wordmark/wordmark-teal.svg";
import { setAuthToken, setCurrentUser } from "@/utils/auth";

const SignInPage: NextPageWithLayout = () => {
  const router = useRouter();

  const [errors, setErrors] = useState<MutationError[]>([]);

  const submitSignIn = useSignInWithPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ signInWithPassword }) => {
      if (signInWithPassword) {
        const { token, user, errors } = signInWithPassword;
        console.log("token, user, errors", token, user, errors);

        if (errors) {
          setErrors(
            errors.filter((error) => error !== null) as MutationError[],
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
      console.log("Sign in error", error);
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
                sx={{ marginBottom: 4, overflow: "hidden" }}
              >
                <Image src={logo} alt="Orcasound" width={350} />
              </Box>
              <SigninForm
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
