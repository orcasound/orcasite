import { User } from "@/graphql/generated";

export const setCurrentUser = (user: User) => {
  localStorage.setItem("orcasound:user", JSON.stringify(user));
  return user;
};

export const getCurrentUser = () => {
  const user = localStorage.getItem("orcasound:user");
  if (user) {
    return JSON.parse(user) as User;
  }
};

export const setAuthToken = (token: string) => {
  localStorage.setItem("orcasound:auth_token", token);
};
export const getAuthToken = () => {
  return localStorage.getItem("orcasound:auth_token");
};
