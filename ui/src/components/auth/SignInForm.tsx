import { Alert, Box, Button, Link, TextField } from "@mui/material";
import NextLink from "next/link";
import React, { useState } from "react";

interface SignInFormProps {
  onSubmit: (email: string, password: string) => void;
  errors?: string[];
}

const SignInForm: React.FC<SignInFormProps> = ({ onSubmit, errors }) => {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    onSubmit(email, password);
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

      <Box display="flex" justifyContent="space-between">
        <Link
          component={NextLink}
          variant="body2"
          sx={{
            textDecoration: "none",
            color: (theme) => theme.palette.accent4.main,
            marginRight: 2,
          }}
          href="/password-reset"
        >
          Forgot your password?
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
        Sign In
      </Button>
    </form>
  );
};

const errorCodeToMessage = (error: string) => {
  if (error === "invalid_credentials") {
    return "Your email and password didn't match our records. Please try again.";
  } else if (error) {
    return `An error occurred: ${error}. Please try again and let us know if this keeps happening.`;
  } else {
    return `An unknown error occurred. Please try again and let us know if this keeps happening.`;
  }
};

export default SignInForm;
