import { zodResolver } from "@hookform/resolvers/zod";
import { Box, Checkbox, FormControlLabel, TextField } from "@mui/material";
import { useForm } from "react-hook-form";
import { z } from "zod";

import { createFormSubmitHandler, StepFormProps } from "../utils";
import { FormActions } from "./FormActions";

const profileSchema = z.object({
  username: z.string().optional(),
  firstName: z.string().optional(),
  lastName: z.string().optional(),
  isMarineScientist: z.boolean().default(false),
  organization: z.string().optional(),
});

type ProfileFormInputs = z.infer<typeof profileSchema>;

const useProfileForm = (onSuccess: () => void) => {
  const form = useForm<ProfileFormInputs>({
    resolver: zodResolver(profileSchema),
    defaultValues: {
      isMarineScientist: false,
    },
  });

  const onSubmit = (data: ProfileFormInputs) => {
    // TODO: Implement profile update mutation
    onSuccess();
  };

  return { form, onSubmit };
};

export const ProfileStep = ({ onSuccess, onSkip }: StepFormProps) => {
  const { form, onSubmit } = useProfileForm(onSuccess);
  const { register, watch } = form;
  const isMarineScientist = watch("isMarineScientist");

  return (
    <form onSubmit={createFormSubmitHandler(form, onSubmit)} name="profile">
      <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
        <TextField
          {...register("username")}
          label="Username"
          fullWidth
          helperText="Choose a unique username for the community"
        />

        <TextField {...register("firstName")} label="First name" fullWidth />

        <TextField {...register("lastName")} label="Last name" fullWidth />

        <FormControlLabel
          control={
            <Checkbox {...register("isMarineScientist")} color="primary" />
          }
          label="I am a professional marine scientist"
        />

        {isMarineScientist && (
          <TextField
            {...register("organization")}
            label="Organization"
            fullWidth
            helperText="Your professional affiliation"
          />
        )}

        <FormActions onSkip={onSkip} />
      </Box>
    </form>
  );
};
