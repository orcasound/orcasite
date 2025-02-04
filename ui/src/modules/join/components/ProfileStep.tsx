import { zodResolver } from "@hookform/resolvers/zod";
import { Checkbox, FormControlLabel, TextField } from "@mui/material";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { z } from "zod";

import {
  MutationError,
  useUpdateUserProfileMutation,
} from "@/graphql/generated";
import { useAuth } from "@/hooks/useAuth";

import { FormContainer } from "../styles";
import {
  createFormSubmitHandler,
  getFieldErrorProps,
  StepFormProps,
} from "../utils";
import { FormActions } from "./FormActions";

const profileSchema = z.object({
  username: z.string().optional(),
  firstName: z.string().optional(),
  lastName: z.string().optional(),
  isScientist: z.boolean().default(false),
  organization: z.string().optional(),
});

type ProfileFormInputs = z.infer<typeof profileSchema>;

const useProfileForm = (onSuccess: () => void) => {
  const [errors, setErrors] = useState<MutationError[]>([]);
  const { user } = useAuth();

  const form = useForm<ProfileFormInputs>({
    resolver: zodResolver(profileSchema),
    defaultValues: {
      isScientist: false,
    },
  });

  const updateProfile = useUpdateUserProfileMutation();

  const onSubmit = (data: ProfileFormInputs) => {
    if (!user) return;
    setErrors([]);

    updateProfile.mutate(
      {
        id: user.id,
        ...data,
      },
      {
        onSuccess: (data) => {
          const { updateUserProfile } = data;
          if (updateUserProfile?.result) {
            onSuccess();
          } else if (updateUserProfile?.errors?.length) {
            setErrors(updateUserProfile.errors);
          } else {
            setErrors([{ message: "An unknown error occurred" }]);
          }
        },
        onError: (error) => {
          console.error("updateProfile error:", error);
          setErrors([{ message: "An unknown error occurred" }]);
        },
      },
    );
  };

  return {
    form,
    onSubmit,
    errors,
  };
};

export const ProfileStep = ({ onSuccess, onSkip }: StepFormProps) => {
  const { form, onSubmit, errors } = useProfileForm(onSuccess);
  const {
    register,
    watch,
    formState: { errors: formErrors },
  } = form;
  const isScientist = watch("isScientist");

  const usernameProps = getFieldErrorProps(
    "username",
    formErrors.username,
    errors,
  );
  const organizationProps = getFieldErrorProps(
    "organization",
    formErrors.organization,
    errors,
  );

  return (
    <form onSubmit={createFormSubmitHandler(form, onSubmit)} name="profile">
      <FormContainer>
        <TextField
          {...register("username")}
          label="Username"
          fullWidth
          error={usernameProps.error}
          helperText={
            usernameProps.helperText ||
            "Choose a unique username for the community"
          }
        />

        <TextField
          {...register("firstName")}
          label="First name"
          fullWidth
          {...getFieldErrorProps("firstName", formErrors.firstName, errors)}
        />

        <TextField
          {...register("lastName")}
          label="Last name"
          fullWidth
          {...getFieldErrorProps("lastName", formErrors.lastName, errors)}
        />

        <FormControlLabel
          control={<Checkbox {...register("isScientist")} color="primary" />}
          label="I am a professional marine scientist"
        />

        {isScientist && (
          <TextField
            {...register("organization")}
            label="Organization"
            fullWidth
            error={organizationProps.error}
            helperText={
              organizationProps.helperText || "Your professional affiliation"
            }
          />
        )}

        <FormActions onSkip={onSkip} />
      </FormContainer>
    </form>
  );
};
