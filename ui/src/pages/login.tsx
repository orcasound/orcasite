import { useRouter } from "next/router";
import { useEffect } from "react";

import { getMapLayout } from "@/components/layouts/MapLayout";
import { useAuth } from "@/hooks/useAuth";
import { LoginStep, StepLayout } from "@/modules/join";
import type { NextPageWithLayout } from "@/pages/_app";

const LoginPage: NextPageWithLayout = () => {
  const router = useRouter();
  const { user, isLoadingUser } = useAuth();

  useEffect(() => {
    if (!router.isReady) return;
    if (isLoadingUser) return;

    // Redirect logged in users to home page
    if (user) router.replace("/");
  }, [user, router, isLoadingUser]);

  if (isLoadingUser) {
    return <div>Loading...</div>;
  }

  if (user) return null;

  return (
    <StepLayout
      pageTitle="Welcome Back"
      title="Sign in to your account"
      description="Continue your journey with the Orcasound community"
    >
      <LoginStep onSuccess={() => router.push("/")} />
    </StepLayout>
  );
};

LoginPage.getLayout = getMapLayout;

export default LoginPage;
