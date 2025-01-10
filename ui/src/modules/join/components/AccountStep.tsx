import { zodResolver } from "@hookform/resolvers/zod";
import { Box, TextField } from "@mui/material";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { z } from "zod";

import { MutationError } from "@/graphql/generated";
import { useAuth } from "@/hooks/useAuth";

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

  const onSubmit = async (data: AccountFormInputs) => {
    try {
      setErrors([]);
      await registerUser({
        email: data.email,
        password: data.password,
        passwordConfirmation: data.password,
      });
      onSuccess();
    } catch (errors) {
      if (Array.isArray(errors)) {
        setErrors(
          errors.filter((error): error is MutationError => error !== null),
        );
      } else {
        console.error("Register error:", errors);
      }
    }
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
      <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
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
      </Box>
    </form>
  );
};
