import { useQueryClient } from "@tanstack/react-query";

import {
  useGetCurrentUserQuery,
  useRegisterWithPasswordMutation,
  useSignInWithPasswordMutation,
  useSignOutMutation,
} from "@/graphql/generated";

export function useAuth() {
  const queryClient = useQueryClient();

  const { data: { currentUser } = {}, isLoading: isLoadingUser } =
    useGetCurrentUserQuery();
  const { mutate: signIn } = useSignInWithPasswordMutation({
    onSuccess: ({ signInWithPassword }) => {
      queryClient.setQueryData(useGetCurrentUserQuery.getKey(), {
        currentUser: signInWithPassword?.user,
      });
    },
  });
  const { mutate: signOut } = useSignOutMutation({
    onSuccess: () => {
      queryClient.removeQueries({ queryKey: useGetCurrentUserQuery.getKey() });
    },
  });
  const { mutate: register } = useRegisterWithPasswordMutation({
    onSuccess: ({ registerWithPassword }) => {
      queryClient.setQueryData(useGetCurrentUserQuery.getKey(), {
        currentUser: registerWithPassword?.result,
      });
    },
  });

  return {
    user: currentUser,
    isLoadingUser,
    isAuthenticated: !!currentUser,
    signIn,
    signOut,
    register,
  };
}
