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

const accountSchema = z.object({
  email: z.string().min(1, "Email is required").email("Invalid email address"),
  password: z
    .string()
    .min(1, "Password is required")
    .min(8, "Password must be at least 8 characters"),
});

type AccountFormInputs = z.infer<typeof accountSchema>;

const useAccountForm = (onSuccess: () => void) => {
  const [errors, setErrors] = useState<MutationError[]>([]);
  const form = useForm<AccountFormInputs>({
    resolver: zodResolver(accountSchema),
  });
  const { register: registerUser } = useAuth();

  const onSubmit = (data: AccountFormInputs) => {
    setErrors([]);
    registerUser(
      {
        email: data.email,
        password: data.password,
        passwordConfirmation: data.password,
      },
      {
        onSuccess: (data) => {
          const { registerWithPassword } = data;
          if (registerWithPassword?.result) {
            onSuccess();
          } else if (registerWithPassword?.errors?.length) {
            setErrors(registerWithPassword.errors);
          } else {
            setErrors([{ message: "An unknown error occurred" }]);
          }
        },
        onError: (error) => {
          console.error("register error:", error);
          setErrors([{ message: "An unknown error occurred" }]);
        },
      },
    );
  };

  return { form, errors, onSubmit };
};

type AccountStepProps = Omit<StepFormProps, "onSkip">;

export const AccountStep = ({ onSuccess }: AccountStepProps) => {
  const { form, errors, onSubmit } = useAccountForm(onSuccess);
  const {
    register,
    formState: { errors: formErrors },
  } = form;

  return (
    <form
      onSubmit={createFormSubmitHandler(form, onSubmit)}
      noValidate
      name="account"
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

        <FormActions submitText="Create account" />

        <Box sx={{ display: "flex", justifyContent: "center" }}>
          <FormActionLink href="/login">
            Already have an account?
          </FormActionLink>
        </Box>
      </FormContainer>
    </form>
  );
};
