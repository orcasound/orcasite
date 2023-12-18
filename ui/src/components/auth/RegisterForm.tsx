import { Alert, Box, Button, TextField } from "@mui/material";
import { FormEvent, useState } from "react";

import Link from "@/components/Link";
import {
  MutationError,
  RegisterWithPasswordMutationVariables,
} from "@/graphql/generated";

type RegisterFormProps = {
  onSubmit: (args: RegisterWithPasswordMutationVariables) => void;
  errors: MutationError[];
};

export default function RegisterForm({ onSubmit, errors }: RegisterFormProps) {
  const [firstName, setFirstName] = useState("");
  const [lastName, setLastName] = useState("");
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [passwordConfirmation, setPasswordConfirmation] = useState("");

  const handleSubmit = (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    onSubmit({ firstName, lastName, email, password, passwordConfirmation });
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
          href="/sign-in"
        >
          Already have an account?
        </Link>
      </Box>
    </form>
  );
}

const errorCodeToMessage = (_error: Pick<MutationError, "code">) => {
  return "An unknown error occurred. Please try again and let us know if this keeps happening.";
};
