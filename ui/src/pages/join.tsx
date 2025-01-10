import { useRouter } from "next/router";
import { useEffect } from "react";

import { getMapLayout } from "@/components/layouts/MapLayout";
import { useAuth } from "@/hooks/useAuth";
import {
  AccountStep,
  PreferencesStep,
  ProfileStep,
  Step,
  StepLayout,
  SuccessStep,
  useSteps,
} from "@/modules/join";
import type { NextPageWithLayout } from "@/pages/_app";

type StepConfig = {
  pageTitle: string;
  title: string;
  description: string;
};

const stepConfigs: Record<Step, StepConfig> = {
  account: {
    pageTitle: "Create an Account",
    title: "Create an account",
    description:
      "Join the Orcasound community to help track and protect marine life",
  },
  profile: {
    pageTitle: "Complete Your Profile",
    title: "Tell us about yourself",
    description: "Help us personalize your experience (optional)",
  },
  preferences: {
    pageTitle: "Set Your Preferences",
    title: "Choose how to engage",
    description:
      "Select your preferences for participating in the Orcasound community",
  },
  success: {
    pageTitle: "Welcome to Orcasound",
    title: "You are all set!",
    description:
      "Welcome to the Orcasound community. You can now start listening for whales and helping protect marine life",
  },
};

const JoinPage: NextPageWithLayout = () => {
  const router = useRouter();
  const { step, setStep, goToNextStep, skipToNextStep } = useSteps();
  const { user, isLoadingUser } = useAuth();

  // Decide which step to show based on the user's state and the desired step
  useEffect(() => {
    if (!router.isReady) return;
    if (isLoadingUser) return;

    // Start the join flow if the user is not logged in
    if (!user && !step) setStep("account");
    // Logged in users should not be able to access the join page, so redirect
    else if (user && !step) router.replace("/");
    // Unless they're trying to directly access a step (but skip the account creation step)
    else if (user && step === "account") goToNextStep();
  }, [user, step, router, goToNextStep, setStep, isLoadingUser]);

  if (isLoadingUser) {
    return <div>Loading...</div>;
  }

  if (!step) return null;

  const currentConfig = stepConfigs[step];

  return (
    <div>
      <main>
        <StepLayout
          pageTitle={currentConfig.pageTitle}
          title={currentConfig.title}
          description={currentConfig.description}
        >
          {step === "account" && <AccountStep onSuccess={goToNextStep} />}
          {step === "profile" && (
            <ProfileStep onSuccess={goToNextStep} onSkip={skipToNextStep} />
          )}
          {step === "preferences" && (
            <PreferencesStep onSuccess={goToNextStep} onSkip={skipToNextStep} />
          )}
          {step === "success" && <SuccessStep />}
        </StepLayout>
      </main>
    </div>
  );
};

JoinPage.getLayout = getMapLayout;

export default JoinPage;
