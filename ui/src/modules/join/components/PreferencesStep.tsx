import { zodResolver } from "@hookform/resolvers/zod";
import { FormControlLabel, Switch } from "@mui/material";
import { useForm } from "react-hook-form";
import { z } from "zod";

import { useUpdateUserPreferencesMutation } from "@/graphql/generated";
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
  const { user } = useAuth();

  const form = useForm<PreferencesFormInputs>({
    resolver: zodResolver(preferencesSchema),
    defaultValues: {
      liveNotifications: false,
      volunteering: false,
      userTesting: false,
      newsletter: false,
    },
  });

  const updatePreferences = useUpdateUserPreferencesMutation({
    onSuccess: ({ updateUserPreferences }) => {
      if (updateUserPreferences?.result) {
        onSuccess();
      }
    },
  });

  const onSubmit = (data: PreferencesFormInputs) => {
    if (!user) return;

    updatePreferences.mutate({
      id: user.id,
      ...data,
    });
  };

  return { form, onSubmit };
};

export const PreferencesStep = ({ onSuccess, onSkip }: StepFormProps) => {
  const { form, onSubmit } = usePreferencesForm(onSuccess);
  const { register } = form;

  return (
    <form onSubmit={createFormSubmitHandler(form, onSubmit)} name="preferences">
      <FormContainer>
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
