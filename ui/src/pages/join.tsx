import { Box, Button, Container, TextField, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/navigation";
import { useState } from "react";

import { getMapLayout } from "../components/layouts/MapLayout";
import {
  MutationError,
  useRegisterWithPasswordMutation,
} from "../graphql/generated";
import type { NextPageWithLayout } from "./_app";

const JoinPage: NextPageWithLayout = () => {
  const router = useRouter();
  const [step] = useState(1);
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [errors, setErrors] = useState<MutationError[]>([]);

  const submitRegister = useRegisterWithPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ registerWithPassword }) => {
      if (registerWithPassword) {
        const { result: user, errors } = registerWithPassword;

        if (errors) {
          setErrors(
            errors.filter((error): error is MutationError => error !== null),
          );
        }

        if (user) {
          // Will add more steps later
          router.push("/");
        }
      }
    },
    onError: (error) => {
      console.log("Register error", error);
    },
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    submitRegister.mutate({
      email,
      password,
      passwordConfirmation: password, // Required by API
    });
  };

  return (
    <div>
      <Head>
        <title>Join Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Box sx={{ mt: 4, mb: 6 }}>
            <Typography variant="h4" gutterBottom>
              Join Orcasound
            </Typography>
            <Typography variant="body1" color="text.secondary">
              Step {step} of 3: Create your account
            </Typography>
          </Box>

          <form onSubmit={handleSubmit}>
            <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
              <TextField
                label="Email"
                type="email"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                required
                fullWidth
                error={errors.some((e) => e.fields?.includes("email"))}
                helperText={
                  errors.find((e) => e.fields?.includes("email"))?.message || ""
                }
              />

              <TextField
                label="Password"
                type="password"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                required
                fullWidth
                error={errors.some((e) => e.fields?.includes("password"))}
                helperText={
                  errors.find((e) => e.fields?.includes("password"))?.message ||
                  ""
                }
              />

              <Button
                type="submit"
                variant="contained"
                color="primary"
                size="large"
                fullWidth
                sx={{ mt: 2 }}
              >
                Continue
              </Button>
            </Box>
          </form>
        </Container>
      </main>
    </div>
  );
};

JoinPage.getLayout = getMapLayout;

export default JoinPage;
