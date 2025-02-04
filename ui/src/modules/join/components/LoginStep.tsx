import { zodResolver } from "@hookform/resolvers/zod";
import { Box, TextField } from "@mui/material";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { z } from "zod";

import { MutationError } from "@/graphql/generated";
import { useAuth } from "@/hooks/useAuth";

import { FormActionLink, FormContainer } from "../styles";
import {
  createFormSubmitHandler,
  getFieldErrorProps,
  StepFormProps,
} from "../utils";
import { FormActions } from "./FormActions";

const loginSchema = z.object({
  email: z.string().min(1, "Email is required").email("Invalid email address"),
  password: z.string().min(1, "Password is required"),
});

type LoginFormInputs = z.infer<typeof loginSchema>;

const useLoginForm = (onSuccess: () => void) => {
  const [errors, setErrors] = useState<MutationError[]>([]);
  const form = useForm<LoginFormInputs>({
    resolver: zodResolver(loginSchema),
  });
  const { signIn } = useAuth();

  const onSubmit = (data: LoginFormInputs) => {
    setErrors([]);
    signIn(
      {
        email: data.email,
        password: data.password,
      },
      {
        onSuccess: (data) => {
          const { signInWithPassword } = data;
          if (signInWithPassword?.user) {
            onSuccess();
          } else if (signInWithPassword?.errors?.length) {
            setErrors(signInWithPassword.errors);
          } else {
            setErrors([{ message: "An unknown error occurred" }]);
          }
        },
        onError: (error) => {
          console.error("login error:", error);
          setErrors([{ message: "An unknown error occurred" }]);
        },
      },
    );
  };

  return { form, errors, onSubmit };
};

type LoginStepProps = Omit<StepFormProps, "onSkip">;

export const LoginStep = ({ onSuccess }: LoginStepProps) => {
  const { form, errors, onSubmit } = useLoginForm(onSuccess);
  const {
    register,
    formState: { errors: formErrors },
  } = form;

  return (
    <form
      onSubmit={createFormSubmitHandler(form, onSubmit)}
      noValidate
      name="login"
    >
      <FormContainer>
        <TextField
          {...register("email")}
          label="Email"
          type="email"
          fullWidth
          {...getFieldErrorProps("email", formErrors.email, errors)}
        />

        <TextField
          {...register("password")}
          label="Password"
          type="password"
          fullWidth
          {...getFieldErrorProps("password", formErrors.password, errors)}
        />

        <FormActions submitText="Log in" />

        <Box sx={{ display: "flex", justifyContent: "space-between" }}>
          <FormActionLink href="/password-reset">
            Forgot your password?
          </FormActionLink>
          <FormActionLink href="/join">Need an account?</FormActionLink>
        </Box>
      </FormContainer>
    </form>
  );
};
