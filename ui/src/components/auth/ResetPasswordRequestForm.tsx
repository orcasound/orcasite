import { Alert, Box, Button, TextField } from "@mui/material";
import { FormEvent, useState } from "react";

import Link from "@/components/Link";
import { RequestPasswordResetMutationVariables } from "@/graphql/generated";

type ForgotPasswordFormProps = {
  onSubmit: (args: RequestPasswordResetMutationVariables) => void;
  message?: string;
};

export default function ForgotPasswordForm({
  onSubmit,
  message,
}: ForgotPasswordFormProps) {
  const [email, setEmail] = useState("");

  const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    onSubmit({ email });
    setEmail("");
  };

  return (
    <form onSubmit={handleSubmit}>
      <TextField
        label="Email"
        variant="outlined"
        margin="normal"
        type="email"
        fullWidth
        value={email}
        onChange={(event) => setEmail(event.target.value)}
        sx={{
          "& .MuiFormLabel-root": {
            color: (theme) => theme.palette.secondary.light,
          },
          "& .MuiFormLabel-root.Mui-focused": {
            color: (theme) => theme.palette.secondary.dark,
          },
          "& .MuiOutlinedInput-root": {
            "& fieldset": {
              borderColor: (theme) => theme.palette.accent2.main,
            },
            "&:hover fieldset": {
              borderColor: (theme) => theme.palette.accent2.dark,
            },
            "&.Mui-focused fieldset": {
              borderColor: (theme) => theme.palette.accent2.dark,
            },
          },
        }}
      />
      {message && (
        <Alert sx={{ marginTop: 2, marginBottom: 2 }}>{message}</Alert>
      )}

      <Button
        type="submit"
        variant="contained"
        color="primary"
        size="large"
        fullWidth
        sx={{
          marginTop: 2,
          backgroundColor: (theme) => theme.palette.accent4.main,
          "&:hover": { backgroundColor: (theme) => theme.palette.accent4.dark },
        }}
      >
        Request password reset
      </Button>

      <Box
        sx={{
          display: "flex",
          gap: 1,
          mt: 2,
          justifyContent: "space-between",
          flexDirection: { xs: "column", sm: "row" },
        }}
      >
        <Link
          variant="body2"
          sx={{
            textDecoration: "none",
            color: (theme) => theme.palette.accent4.main,
            marginRight: 2,
            textAlign: { xs: "center", sm: "left" },
          }}
          href="/sign-in"
        >
          Already have an account?
        </Link>
        <Link
          variant="body2"
          sx={{
            textDecoration: "none",
            color: (theme) => theme.palette.accent4.main,
            textAlign: { xs: "center", sm: "right" },
          }}
          href="/register"
        >
          Need an account?
        </Link>
      </Box>
    </form>
  );
}
