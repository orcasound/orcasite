import { Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/navigation";
import { useEffect } from "react";

import { getAuthLayout } from "@/components/layouts/AuthLayout";
import { useSignOutMutation } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const SignOutPage: NextPageWithLayout = () => {
  const router = useRouter();
  const { mutate: signOutMutate } = useSignOutMutation({
    onSuccess: () => {
      router.push("/");
    },
  });

  useEffect(() => {
    signOutMutate({});
  }, [signOutMutate]);

  return (
    <div>
      <Head>
        <title>Sign out | Orcasound</title>
      </Head>

      <main>
        <Typography variant="body1" textAlign="center">
          Signing out...
        </Typography>
      </main>
    </div>
  );
};

SignOutPage.getLayout = getAuthLayout;

export default SignOutPage;
