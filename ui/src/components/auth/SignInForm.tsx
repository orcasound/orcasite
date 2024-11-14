import { Alert, Box, Button, TextField } from "@mui/material";
import { FormEvent, useState } from "react";

import Link from "@/components/Link";
import { SignInWithPasswordMutationVariables } from "@/graphql/generated";

type SignInFormProps = {
  onSubmit: (args: SignInWithPasswordMutationVariables) => void;
  errors?: string[];
};

export default function SignInForm({ onSubmit, errors }: SignInFormProps) {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");

  const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    onSubmit({ email, password });
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
      <TextField
        label="Password"
        variant="outlined"
        margin="normal"
        fullWidth
        type="password"
        value={password}
        onChange={(event) => setPassword(event.target.value)}
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
      {errors &&
        errors.map((error) => (
          <Alert
            severity="error"
            key={error}
            sx={{ marginTop: 2, marginBottom: 2 }}
          >
            {errorCodeToMessage(error)}
          </Alert>
        ))}

      <Button
        type="submit"
        variant="contained"
        size="large"
        fullWidth
        sx={{
          marginTop: 2,
          backgroundColor: (theme) => theme.palette.accent4.main,
          "&:hover": { backgroundColor: (theme) => theme.palette.accent4.dark },
        }}
      >
        Sign In
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
          href="/password-reset"
        >
          Forgot your password?
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

const errorCodeToMessage = (error: string) => {
  if (error === "Authentication failed") {
    return "Your email and password didn't match our records. Please try again.";
  } else if (error) {
    return `An error occurred: ${error}. Please try again and let us know if this keeps happening.`;
  } else {
    return `An unknown error occurred. Please try again and let us know if this keeps happening.`;
  }
};
