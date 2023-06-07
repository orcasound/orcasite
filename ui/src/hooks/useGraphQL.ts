import { type TypedDocumentNode } from '@graphql-typed-document-node/core'
import { useQuery, type UseQueryResult } from '@tanstack/react-query'
import request from 'graphql-request'

// TODO: get API URL from env
const API_ENDPOINT_STATIC = 'https://live.orcasound.net/graphql'

export function useGraphQL<TResult, TVariables>(
  document: TypedDocumentNode<TResult, TVariables>, // TODO: switch to TypedDocumentString
  ...[variables]: TVariables extends Record<string, never> ? [] : [TVariables]
): UseQueryResult<TResult> {
  return useQuery(
    [(document.definitions[0] as any).name.value, variables],
    async ({ queryKey }) =>
      request(
        API_ENDPOINT_STATIC,
        document,
        queryKey[1] ? queryKey[1] : undefined
      )
  )
}
