import { FieldError, UseFormReturn } from "react-hook-form";

import { MutationError } from "@/graphql/generated";

export type StepFormProps = {
  onSuccess: () => void;
  onSkip?: () => void;
};

export const createFormSubmitHandler = <T extends object>(
  form: UseFormReturn<T>,
  onSubmit: (data: T) => void,
) => {
  return form.handleSubmit((data) => {
    console.log("Form data:", data);
    onSubmit(data);
  });
};

export const getFieldErrorProps = (
  field: string,
  formError?: FieldError,
  mutationErrors?: MutationError[],
) => {
  const hasError =
    !!formError || mutationErrors?.some((e) => e.fields?.includes(field));
  const errorMessage =
    formError?.message ||
    mutationErrors?.find((e) => e.fields?.includes(field))?.message ||
    "";

  return {
    error: hasError,
    helperText: errorMessage || "",
  };
};
