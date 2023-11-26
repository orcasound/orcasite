import Head from "next/head";
import { useRouter } from "next/navigation";
import { useState } from "react";

import RegisterForm from "@/components/Auth/RegisterForm";
import { getAuthLayout } from "@/components/layouts/AuthLayout";
import {
  MutationError,
  User,
  useRegisterWithPasswordMutation,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { setAuthToken, setCurrentUser } from "@/utils/auth";

const RegisterPage: NextPageWithLayout = () => {
  const router = useRouter();

  const [errors, setErrors] = useState<MutationError[]>([]);

  const submitRegister = useRegisterWithPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ registerWithPassword }) => {
      if (registerWithPassword) {
        const { metadata, result: user, errors } = registerWithPassword;
        const token = metadata?.token;

        if (errors) {
          setErrors(
            errors.filter((error): error is MutationError => error !== null),
          );
        }

        if (token) {
          setAuthToken(token);
        }

        if (user) {
          setCurrentUser(user as User);
          router.push("/");
        }
      }
    },
    onError: (error) => {
      console.log("Register error", error);
    },
  });

  return (
    <div>
      <Head>
        <title>Register | Orcasound</title>
      </Head>

      <main>
        <RegisterForm
          onSubmit={(
            firstName,
            lastName,
            email,
            password,
            passwordConfirmation,
          ) =>
            submitRegister.mutate({
              firstName,
              lastName,
              email,
              password,
              passwordConfirmation,
            })
          }
          errors={errors}
        />
      </main>
    </div>
  );
};

RegisterPage.getLayout = getAuthLayout;

export default RegisterPage;
