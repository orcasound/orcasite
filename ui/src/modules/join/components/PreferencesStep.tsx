import { zodResolver } from "@hookform/resolvers/zod";
import { Alert, FormControlLabel, Switch } from "@mui/material";
import { useQueryClient } from "@tanstack/react-query";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { z } from "zod";

import {
  GetCurrentUserQuery,
  MutationError,
  useGetCurrentUserQuery,
  useUpdateUserPreferencesMutation,
} from "@/graphql/generated";
import { useAuth } from "@/hooks/useAuth";

import { FormContainer } from "../styles";
import { createFormSubmitHandler, StepFormProps } from "../utils";
import { FormActions } from "./FormActions";

const preferencesSchema = z.object({
  liveNotifications: z.boolean().default(false),
  volunteering: z.boolean().default(false),
  userTesting: z.boolean().default(false),
  newsletter: z.boolean().default(false),
});

type PreferencesFormInputs = z.infer<typeof preferencesSchema>;

const usePreferencesForm = (onSuccess: () => void) => {
  const [errors, setErrors] = useState<MutationError[]>([]);
  const { user } = useAuth();
  const queryClient = useQueryClient();

  const form = useForm<PreferencesFormInputs>({
    resolver: zodResolver(preferencesSchema),
    defaultValues: {
      liveNotifications: false,
      volunteering: false,
      userTesting: false,
      newsletter: false,
    },
  });

  const updatePreferences = useUpdateUserPreferencesMutation();

  const onSubmit = (data: PreferencesFormInputs) => {
    if (!user) return;
    setErrors([]);

    updatePreferences.mutate(
      {
        id: user.id,
        ...data,
      },
      {
        onSuccess: (data) => {
          const { updateUserPreferences } = data;
          if (updateUserPreferences?.result) {
            queryClient.setQueryData<GetCurrentUserQuery>(
              useGetCurrentUserQuery.getKey(),
              {
                currentUser: {
                  ...user,
                  ...updateUserPreferences.result,
                },
              },
            );
            onSuccess();
          } else if (updateUserPreferences?.errors?.length) {
            setErrors(updateUserPreferences.errors);
          } else {
            setErrors([{ message: "An unknown error occurred" }]);
          }
        },
        onError: (error) => {
          console.error("updatePreferences error:", error);
          setErrors([{ message: "An unknown error occurred" }]);
        },
      },
    );
  };

  return { form, onSubmit, errors };
};

export const PreferencesStep = ({ onSuccess, onSkip }: StepFormProps) => {
  const { form, onSubmit, errors } = usePreferencesForm(onSuccess);
  const { register } = form;

  return (
    <form onSubmit={createFormSubmitHandler(form, onSubmit)} name="preferences">
      <FormContainer>
        {errors?.length > 0 && (
          <Alert severity="error" sx={{ mb: 2 }}>
            {errors[0].message}
          </Alert>
        )}
        <FormControlLabel
          control={<Switch {...register("liveNotifications")} />}
          label="Notify me when orcas are detected live"
        />

        <FormControlLabel
          control={<Switch {...register("volunteering")} />}
          label="I'm interested in volunteering with Orcasound"
        />

        <FormControlLabel
          control={<Switch {...register("userTesting")} />}
          label="I'd like to participate in website user testing"
        />

        <FormControlLabel
          control={<Switch {...register("newsletter")} />}
          label="Subscribe to the Orcasound newsletter"
        />

        <FormActions onSkip={onSkip} submitText="Complete setup" />
      </FormContainer>
    </form>
  );
};
