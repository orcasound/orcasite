import { Alert, Box, Button, Link, TextField } from "@mui/material";
import NextLink from "next/link";
import { FormEvent, useState } from "react";

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

      <Box display="flex" justifyContent="space-between">
        <Link
          component={NextLink}
          variant="body2"
          sx={{
            textDecoration: "none",
            color: (theme) => theme.palette.accent4.main,
            marginRight: 2,
          }}
          href="/sign-in"
        >
          Already have an account?
        </Link>
        <Link
          component={NextLink}
          variant="body2"
          sx={{
            textDecoration: "none",
            color: (theme) => theme.palette.accent4.main,
          }}
          href="/register"
          textAlign="right"
        >
          Need an account?
        </Link>
      </Box>

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
    </form>
  );
}
