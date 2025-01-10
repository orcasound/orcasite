import { useQueryClient } from "@tanstack/react-query";
import { useRouter } from "next/navigation";
import { useCallback } from "react";

import {
  useGetCurrentUserQuery,
  useRegisterWithPasswordMutation,
  useSignInWithPasswordMutation,
  useSignOutMutation,
} from "@/graphql/generated";

type SignInParams = {
  email: string;
  password: string;
};

type RegisterParams = {
  email: string;
  password: string;
  passwordConfirmation: string;
  firstName?: string;
  lastName?: string;
};

export function useAuth() {
  const router = useRouter();
  const queryClient = useQueryClient();

  const { data: { currentUser } = {}, isLoading: isLoadingUser } =
    useGetCurrentUserQuery();
  const { mutateAsync: signInMutation } = useSignInWithPasswordMutation();
  const { mutateAsync: signOutMutation } = useSignOutMutation();
  const { mutateAsync: registerMutation } = useRegisterWithPasswordMutation();

  const refreshSession = useCallback(() => {
    queryClient.invalidateQueries({ queryKey: ["getCurrentUser"] });
  }, [queryClient]);

  const signIn = useCallback(
    async (params: SignInParams) => {
      const { signInWithPassword } = await signInMutation(params);
      if (signInWithPassword?.errors) throw signInWithPassword.errors;
      refreshSession();
      return signInWithPassword?.user;
    },
    [signInMutation, refreshSession],
  );

  const signOut = useCallback(async () => {
    await signOutMutation({});
    queryClient.removeQueries();
    router.push("/");
  }, [signOutMutation, queryClient, router]);

  const register = useCallback(
    async (params: RegisterParams) => {
      const { registerWithPassword } = await registerMutation(params);
      if (registerWithPassword.errors.length > 0)
        throw registerWithPassword.errors;
      refreshSession();
      return registerWithPassword.result;
    },
    [registerMutation, refreshSession],
  );

  return {
    user: currentUser,
    isLoadingUser,
    isAuthenticated: !!currentUser,
    signIn,
    signOut,
    register,
  };
}
