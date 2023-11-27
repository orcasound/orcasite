import Head from "next/head";
import { useRouter } from "next/navigation";
import { useState } from "react";

import SignInForm from "@/components/auth/SignInForm";
import { getAuthLayout } from "@/components/layouts/AuthLayout";
import { useSignInWithPasswordMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { setAuthToken, setCurrentUser } from "@/utils/auth";

const SignInPage: NextPageWithLayout = () => {
  const router = useRouter();

  const [errors, setErrors] = useState<string[]>([]);

  const submitSignIn = useSignInWithPasswordMutation({
    onMutate: () => {
      setErrors([]);
    },
    onSuccess: ({ signInWithPassword }) => {
      if (signInWithPassword) {
        const { token, user, errors } = signInWithPassword;

        if (errors && errors?.length > 0) {
          setErrors(
            errors.flatMap((error) => {
              const message = error?.message || error?.code;
              if (typeof message === "string") {
                return [message];
              }
              return [];
            }),
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
    onError: (error: Error) => {
      setErrors([error.message]);
    },
  });

  return (
    <div>
      <Head>
        <title>Sign in | Orcasound</title>
      </Head>

      <main>
        <SignInForm onSubmit={submitSignIn.mutate} errors={errors} />
      </main>
    </div>
  );
};

SignInPage.getLayout = getAuthLayout;

export default SignInPage;
