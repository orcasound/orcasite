import { Alert, Button, TextField } from "@mui/material";
import React, { useState } from "react";

import { MutationError } from "@/graphql/generated";

interface ResetPasswordFormProps {
  onSubmit: (password: string, passworConfirmation: string) => void;
  errors: MutationError[];
}

const ResetPasswordForm: React.FC<ResetPasswordFormProps> = ({
  onSubmit,
  errors,
}) => {
  const [password, setPassword] = useState("");
  const [passwordConfirmation, setPasswordConfirmation] = useState("");

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    onSubmit(password, passwordConfirmation);
  };

  return (
    <form onSubmit={handleSubmit}>
      <TextField
        label="Password"
        variant="outlined"
        margin="normal"
        fullWidth
        type="password"
        value={password}
        error={!!errors.find((error) => error?.fields?.includes("password"))}
        helperText={errorToString(
          errors.find((error) => error?.fields?.includes("password")),
          true,
        )}
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
      <TextField
        label="Password Confirmation"
        variant="outlined"
        margin="normal"
        fullWidth
        type="password"
        value={passwordConfirmation}
        error={
          !!errors.find(
            (error) => error?.fields?.includes("password_confirmation"),
          )
        }
        helperText={errorToString(
          errors.find(
            (error) => error?.fields?.includes("password_confirmation"),
          ),
          true,
        )}
        onChange={(event) => setPasswordConfirmation(event.target.value)}
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
        errors
          .filter((error) => error?.fields?.length === 0)
          .map((error, index) => (
            <Alert
              severity="error"
              key={index}
              sx={{ marginTop: 2, marginBottom: 2 }}
            >
              {errorToString(error)}
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
        Reset password
      </Button>
    </form>
  );
};

const errorToString = (
  error?: Pick<MutationError, "code" | "message" | "vars" | "shortMessage">,
  shortMessage: boolean = false,
) => {
  if (!error) {
    return "";
  }

  if (
    typeof error.vars === "object" &&
    error.vars &&
    Object.keys(error.vars).length > 0
  ) {
    const vars = error.vars;
    return Object.keys(error.vars)
      .filter((key) => key !== "message" && key !== "field")
      .reduce(
        (acc, key) => acc.replaceAll(`%{${key}}`, vars[key]),
        error.vars.message,
      );
  }

  if (shortMessage) {
    return error.shortMessage;
  }

  if (error.message) {
    return `An error occurred: ${error.message}. Please try again and let us know if this keeps happening.`;
  } else {
    return `An unknown error occurred. Please try again and let us know if this keeps happening.`;
  }
};

export default ResetPasswordForm;
