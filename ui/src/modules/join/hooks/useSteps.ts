import { useState } from "react";

export const steps = {
  account: 1,
  profile: 2,
  preferences: 3,
  success: 4,
} as const;

type Step = (typeof steps)[keyof typeof steps];

export const useSteps = () => {
  const [currentStep, setCurrentStep] = useState<Step>(steps.account);

  const goToNextStep = () => {
    setCurrentStep((prev) =>
      prev < steps.success ? ((prev + 1) as Step) : prev,
    );
  };

  const skipToNextStep = () => {
    goToNextStep();
  };

  return {
    currentStep,
    goToNextStep,
    skipToNextStep,
  };
};
