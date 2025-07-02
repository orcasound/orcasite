import { useRouter } from "next/router";
import { useCallback } from "react";

const steps = ["account", "profile", "preferences", "success"] as const;
export type Step = (typeof steps)[number];

/**
 * Hook to manage the join steps
 *
 * Uses the query parameter `step` to track the current step
 */
export const useSteps = () => {
  const router = useRouter();
  const step = router.query.step as Step | undefined;

  const setStep = useCallback(
    (newStep: Step, usePush = false) => {
      const routerMethod = usePush ? router.push : router.replace;
      routerMethod({ query: { step: newStep } }, undefined, {
        shallow: true,
      });
    },
    [router],
  );

  const goToNextStep = useCallback(() => {
    if (!step) return;
    const currentIndex = steps.indexOf(step);
    if (currentIndex < steps.length - 1) {
      const nextStep = steps[currentIndex + 1];
      setStep(nextStep, true);
    }
  }, [step, setStep]);

  const skipToNextStep = useCallback(() => {
    goToNextStep();
  }, [goToNextStep]);

  return {
    step,
    setStep,
    goToNextStep,
    skipToNextStep,
  };
};
