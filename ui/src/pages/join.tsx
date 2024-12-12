import { zodResolver } from "@hookform/resolvers/zod";
import { Box, Button, Container, TextField, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/navigation";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { z } from "zod";

import { getMapLayout } from "../components/layouts/MapLayout";
import {
  MutationError,
  useRegisterWithPasswordMutation,
} from "../graphql/generated";
import type { NextPageWithLayout } from "./_app";

const joinSchema = z.object({
  email: z.string().min(1, "Email is required").email("Invalid email address"),
  password: z
    .string()
    .min(1, "Password is required")
    .min(8, "Password must be at least 8 characters")
    .regex(
      /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)/,
      "Password must contain at least one uppercase letter, one lowercase letter, and one number",
    ),
});

type JoinFormInputs = z.infer<typeof joinSchema>;

const JoinPage: NextPageWithLayout = () => {
  const router = useRouter();
  const [step] = useState(1);
  const [errors, setErrors] = useState<MutationError[]>([]);

  const {
    register,
    handleSubmit,
    formState: { errors: formErrors },
  } = useForm<JoinFormInputs>({
    resolver: zodResolver(joinSchema),
  });

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

  const onSubmit = (data: JoinFormInputs) => {
    submitRegister.mutate({
      email: data.email,
      password: data.password,
      passwordConfirmation: data.password,
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

          <form onSubmit={handleSubmit(onSubmit)} noValidate>
            <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
              <TextField
                {...register("email")}
                label="Email"
                type="email"
                fullWidth
                error={
                  !!formErrors.email ||
                  errors.some((e) => e.fields?.includes("email"))
                }
                helperText={
                  formErrors.email?.message ||
                  errors.find((e) => e.fields?.includes("email"))?.message ||
                  ""
                }
              />

              <TextField
                {...register("password")}
                label="Password"
                type="password"
                fullWidth
                error={
                  !!formErrors.password ||
                  errors.some((e) => e.fields?.includes("password"))
                }
                helperText={
                  formErrors.password?.message ||
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
