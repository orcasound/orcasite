import { Box, Button } from "@mui/material";

type FormActionsProps = {
  onSkip?: () => void;
  submitText?: string;
  skipText?: string;
};

export const FormActions = ({
  onSkip,
  submitText = "Continue",
  skipText = "Skip for now",
}: FormActionsProps) => (
  <Box sx={{ display: "flex", gap: 2, mt: 2 }}>
    {onSkip && (
      <Button variant="outlined" onClick={onSkip} fullWidth>
        {skipText}
      </Button>
    )}
    <Button type="submit" variant="contained" color="primary" fullWidth>
      {submitText}
    </Button>
  </Box>
);
