import Head from "next/head";
import { useRouter } from "next/router";
import { useState } from "react";

import ResetPasswordForm from "@/components/auth/ResetPasswordForm";
import { getAuthLayout } from "@/components/layouts/AuthLayout";
import { MutationError, useResetPasswordMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { setAuthToken, setCurrentUser } from "@/utils/auth";

const PasswordResetPage: NextPageWithLayout = () => {
  const router = useRouter();
  const token = router.query.token as string;

  const [errors, setErrors] = useState<MutationError[]>([]);

  const submitResetPassword = useResetPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ resetPassword }) => {
      if (resetPassword) {
        const { token, errors, user } = resetPassword;

        if (errors && errors?.length > 0) {
          setErrors(
            errors.filter((error): error is MutationError => error !== null),
          );
        }

        if (token) {
          setAuthToken(token);
        }

        if (user) {
          setCurrentUser(user);
          router.push("/");
        }
      }
    },
    onError: (error) => {
      console.log("Forgot password error", error);
    },
  });

  return (
    <div>
      <Head>
        <title>Reset password | Orcasound</title>
      </Head>

      <main>
        <ResetPasswordForm
          onSubmit={(password, passwordConfirmation) =>
            submitResetPassword.mutate({
              password,
              passwordConfirmation,
              resetToken: token,
            })
          }
          errors={errors}
        />
      </main>
    </div>
  );
};

PasswordResetPage.getLayout = getAuthLayout;

export default PasswordResetPage;
