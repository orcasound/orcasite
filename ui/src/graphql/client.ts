import { getAuthToken } from "@/utils/auth";

/* eslint-disable import/no-unused-modules */
if (!process.env.NEXT_PUBLIC_GQL_ENDPOINT) {
  throw new Error("NEXT_PUBLIC_GQL_ENDPOINT is not set");
}

export const endpointUrl = process.env.NEXT_PUBLIC_GQL_ENDPOINT;

export const fetchParams = () => {
  const authToken = getAuthToken();
  return {
    headers: {
      "Content-Type": "application/json; charset=utf-8",
      ...(authToken && { Authorization: `Bearer ${authToken}` }),
    },
  };
};

export function fetcher<TData, TVariables>(
  query: string,
  variables?: TVariables,
  options?: RequestInit["headers"],
) {
  return async (): Promise<TData> => {
    const res = await fetch(endpointUrl as string, {
      method: "POST",
      ...fetchParams(),
      ...(options || {}),
      body: JSON.stringify({ query, variables }),
    });

    const json = await res.json();

    if (json.errors) {
      const { message } = json.errors[0];

      throw new Error(message);
    }

    return json.data;
  };
}
