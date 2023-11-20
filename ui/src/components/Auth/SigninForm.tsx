import { Alert, Button, TextField } from "@mui/material";
import React, { useState } from "react";

import { MutationError } from "@/graphql/generated";

interface LoginFormProps {
  onSubmit: (email: string, password: string) => void;
  errors: MutationError[];
}

const SigninForm: React.FC<LoginFormProps> = ({ onSubmit, errors }) => {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    const result = onSubmit(email, password);
    console.log("Signin result", result);
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
        errors.map((error, index) => (
          <Alert severity="error" key={index} sx={{ marginTop: 2, marginBottom: 2 }}>
            {errorCodeToMessage(error)}
          </Alert>
        ))}

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

const errorCodeToMessage = (error: Pick<MutationError, "code">) => {
  if (error.code === "invalid_credentials") {
    return "Your email or password didn't match our records. Please try again.";
  } else {
    return "An unknown error occurred. Please try again and let us know if this keeps happening.";
  }
};

export default SigninForm;
