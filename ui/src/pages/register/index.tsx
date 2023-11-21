import { Box, Paper } from "@mui/material";
import Head from "next/head";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { useState } from "react";

import RegisterForm from "@/components/Auth/RegisterForm";
import Header from "@/components/Header";
import {
  MutationError,
  User,
  useRegisterWithPasswordMutation,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import logo from "@/public/wordmark/wordmark-teal.svg";
import { setAuthToken, setCurrentUser } from "@/utils/auth";

const RegisterPage: NextPageWithLayout = () => {
  const router = useRouter();

  const [errors, setErrors] = useState<MutationError[]>([]);

  const submitRegister = useRegisterWithPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ registerWithPassword }) => {
      if (registerWithPassword) {
        const { metadata, result: user, errors } = registerWithPassword;
        const token = metadata?.token;

        if (errors) {
          setErrors(
            errors.filter((error): error is MutationError => error !== null),
          );
        }

        if (token) {
          setAuthToken(token);
        }

        if (user) {
          setCurrentUser(user as User);
          router.push("/");
        }
      }
    },
    onError: (error) => {
      console.log("Register error", error);
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
            <Paper sx={{ p: 4, maxWidth: { xs: "100%", sm: 500 }, m: 4 }}>
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
              <RegisterForm
                onSubmit={(
                  firstName,
                  lastName,
                  email,
                  password,
                  passwordConfirmation,
                ) =>
                  submitRegister.mutate({
                    firstName,
                    lastName,
                    email,
                    password,
                    passwordConfirmation,
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

export default RegisterPage;
