import { zodResolver } from "@hookform/resolvers/zod";
import { Box, TextField } from "@mui/material";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { z } from "zod";

import {
  MutationError,
  useRegisterWithPasswordMutation,
} from "@/graphql/generated";

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
    .min(8, "Password must be at least 8 characters")
    .regex(
      /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)/,
      "Password must contain at least one uppercase letter, one lowercase letter, and one number",
    ),
});

type AccountFormInputs = z.infer<typeof accountSchema>;

const useAccountForm = (onSuccess: () => void) => {
  const [errors, setErrors] = useState<MutationError[]>([]);
  const form = useForm<AccountFormInputs>({
    resolver: zodResolver(accountSchema),
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
          onSuccess();
        }
      }
    },
    onError: (error) => {
      console.log("Register error", error);
    },
  });

  const onSubmit = (data: AccountFormInputs) => {
    submitRegister.mutate({
      email: data.email,
      password: data.password,
      passwordConfirmation: data.password,
    });
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
