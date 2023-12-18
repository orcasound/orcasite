import Head from "next/head";
import { useState } from "react";

import ResetPasswordRequestForm from "@/components/auth/ResetPasswordRequestForm";
import { getAuthLayout } from "@/components/layouts/AuthLayout";
import { useRequestPasswordResetMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const PasswordResetRequestPage: NextPageWithLayout = () => {
  const [message, setMessage] = useState<string>();

  const submitPasswordResetRequest = useRequestPasswordResetMutation({
    onMutate: () => {
      setMessage(undefined);
    },
    onSuccess: ({ requestPasswordReset }) => {
      if (requestPasswordReset) {
        setMessage("Password reset has been sent to the email provided.");
      }
    },
    onError: (error) => {
      console.log("Forgot password error", error);
    },
  });

  return (
    <div>
      <Head>
        <title>Request password reset | Orcasound</title>
      </Head>

      <main>
        <ResetPasswordRequestForm
          onSubmit={({ email }) => submitPasswordResetRequest.mutate({ email })}
          message={message}
        />
      </main>
    </div>
  );
};

PasswordResetRequestPage.getLayout = getAuthLayout;

export default PasswordResetRequestPage;
