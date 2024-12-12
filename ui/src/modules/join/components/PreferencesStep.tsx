import { zodResolver } from "@hookform/resolvers/zod";
import { Box, FormControlLabel, Switch } from "@mui/material";
import { useForm } from "react-hook-form";
import { z } from "zod";

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
  const form = useForm<PreferencesFormInputs>({
    resolver: zodResolver(preferencesSchema),
    defaultValues: {
      liveNotifications: false,
      volunteering: false,
      userTesting: false,
      newsletter: false,
    },
  });

  const onSubmit = (data: PreferencesFormInputs) => {
    // TODO: Implement preferences update mutation
    onSuccess();
  };

  return { form, onSubmit };
};

export const PreferencesStep = ({ onSuccess, onSkip }: StepFormProps) => {
  const { form, onSubmit } = usePreferencesForm(onSuccess);
  const { register } = form;

  return (
    <form onSubmit={createFormSubmitHandler(form, onSubmit)} name="preferences">
      <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
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
      </Box>
    </form>
  );
};
