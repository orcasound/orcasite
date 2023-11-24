import { Alert, Box, Button, Link, TextField } from "@mui/material";
import NextLink from "next/link";
import React, { useState } from "react";

import { MutationError } from "@/graphql/generated";

interface RegisterFormProps {
  onSubmit: (
    firstName: string,
    lastName: string,
    email: string,
    password: string,
    passwordConfirmation: string,
  ) => void;
  errors: MutationError[];
}

const RegisterForm: React.FC<RegisterFormProps> = ({ onSubmit, errors }) => {
  const [firstName, setFirstName] = useState("");
  const [lastName, setLastName] = useState("");
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [passwordConfirmation, setPasswordConfirmation] = useState("");

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    onSubmit(firstName, lastName, email, password, passwordConfirmation);
  };

  return (
    <form onSubmit={handleSubmit}>
      <TextField
        label="First Name"
        variant="outlined"
        margin="normal"
        fullWidth
        value={firstName}
        error={!!errors.find((error) => error?.fields?.includes("first_name"))}
        helperText={
          errors.find((error) => error?.fields?.includes("first_name"))?.message
        }
        onChange={(event) => setFirstName(event.target.value)}
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
        label="Last Name"
        variant="outlined"
        margin="normal"
        fullWidth
        value={lastName}
        error={!!errors.find((error) => error?.fields?.includes("last_name"))}
        helperText={
          errors.find((error) => error?.fields?.includes("last_name"))?.message
        }
        onChange={(event) => setLastName(event.target.value)}
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
        label="Email"
        variant="outlined"
        margin="normal"
        type="email"
        fullWidth
        value={email}
        onChange={(event) => setEmail(event.target.value)}
        error={!!errors.find((error) => error?.fields?.includes("email"))}
        helperText={
          errors.find((error) => error?.fields?.includes("email"))?.message
        }
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
        error={!!errors.find((error) => error?.fields?.includes("password"))}
        helperText={
          errors.find((error) => error?.fields?.includes("password"))?.message
        }
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
        label="Password confirmation"
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
        helperText={
          errors.find(
            (error) => error?.fields?.includes("password_confirmation"),
          )?.message
        }
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
              {errorCodeToMessage(error)}
            </Alert>
          ))}

      <Box
        display="flex"
        justifyContent="space-between"
        sx={{ marginTop: 2, marginBottom: 1 }}
      >
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
          href="/sign-in"
          textAlign="right"
        >
          Already have an account?
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
        Register
      </Button>
    </form>
  );
};

const errorCodeToMessage = (_error: Pick<MutationError, "code">) => {
  return "An unknown error occurred. Please try again and let us know if this keeps happening.";
};

export default RegisterForm;