import { getMapLayout } from "@/components/layouts/MapLayout";
import {
  AccountStep,
  PreferencesStep,
  ProfileStep,
  StepLayout,
  steps,
  SuccessStep,
  useSteps,
} from "@/modules/join";
import type { NextPageWithLayout } from "@/pages/_app";

type StepConfig = {
  pageTitle: string;
  title: string;
  description: string;
};

const stepConfigs: Record<keyof typeof steps, StepConfig> = {
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
  const { currentStep, goToNextStep, skipToNextStep } = useSteps();
  const currentConfig =
    stepConfigs[Object.keys(steps)[currentStep - 1] as keyof typeof steps];

  return (
    <div>
      <main>
        <StepLayout
          pageTitle={currentConfig.pageTitle}
          title={currentConfig.title}
          description={currentConfig.description}
        >
          {currentStep === steps.account && (
            <AccountStep onSuccess={goToNextStep} />
          )}
          {currentStep === steps.profile && (
            <ProfileStep onSuccess={goToNextStep} onSkip={skipToNextStep} />
          )}
          {currentStep === steps.preferences && (
            <PreferencesStep onSuccess={goToNextStep} onSkip={skipToNextStep} />
          )}
          {currentStep === steps.success && <SuccessStep />}
        </StepLayout>
      </main>
    </div>
  );
};

JoinPage.getLayout = getMapLayout;

export default JoinPage;
