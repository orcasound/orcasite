import { endpointUrl, fetchParams } from '@/graphql/client'
import {
  useMutation,
  useQuery,
  UseMutationOptions,
  UseQueryOptions,
} from '@tanstack/react-query'
export type Maybe<T> = T | null
export type InputMaybe<T> = Maybe<T>
export type Exact<T extends { [key: string]: unknown }> = {
  [K in keyof T]: T[K]
}
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & {
  [SubKey in K]?: Maybe<T[SubKey]>
}
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & {
  [SubKey in K]: Maybe<T[SubKey]>
}
export type MakeEmpty<
  T extends { [key: string]: unknown },
  K extends keyof T
> = { [_ in K]?: never }
export type Incremental<T> =
  | T
  | {
      [P in keyof T]?: P extends ' $fragmentName' | '__typename' ? T[P] : never
    }

function fetcher<TData, TVariables>(query: string, variables?: TVariables) {
  return async (): Promise<TData> => {
    const res = await fetch(endpointUrl as string, {
      method: 'POST',
      ...fetchParams,
      body: JSON.stringify({ query, variables }),
    })

    const json = await res.json()

    if (json.errors) {
      const { message } = json.errors[0]

      throw new Error(message)
    }

    return json.data
  }
}
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: { input: string; output: string }
  String: { input: string; output: string }
  Boolean: { input: boolean; output: boolean }
  Int: { input: number; output: number }
  Float: { input: number; output: number }
  DateTime: { input: any; output: any }
  Decimal: { input: any; output: any }
  Json: { input: any; output: any }
}

export type Candidate = {
  __typename?: 'Candidate'
  detectionCount?: Maybe<Scalars['Int']['output']>
  detections?: Maybe<Array<Maybe<Detection>>>
  feed?: Maybe<Feed>
  id?: Maybe<Scalars['ID']['output']>
  maxTime?: Maybe<Scalars['DateTime']['output']>
  minTime?: Maybe<Scalars['DateTime']['output']>
}

/** Pagination version of candidates */
export type Candidates = {
  __typename?: 'Candidates'
  entries?: Maybe<Array<Maybe<Candidate>>>
  meta?: Maybe<PaginationMeta>
}

export type Detection = {
  __typename?: 'Detection'
  description?: Maybe<Scalars['String']['output']>
  feed?: Maybe<Feed>
  id?: Maybe<Scalars['ID']['output']>
  listenerCount?: Maybe<Scalars['Int']['output']>
  playerOffset?: Maybe<Scalars['Decimal']['output']>
  playlistTimestamp?: Maybe<Scalars['Int']['output']>
  sourceIp?: Maybe<Scalars['String']['output']>
  timestamp?: Maybe<Scalars['DateTime']['output']>
}

export type DetectionWithLockout = {
  __typename?: 'DetectionWithLockout'
  detection?: Maybe<Detection>
  lockoutInitial?: Maybe<Scalars['Float']['output']>
  lockoutRemaining?: Maybe<Scalars['Float']['output']>
}

/** Pagination version of detections */
export type Detections = {
  __typename?: 'Detections'
  entries?: Maybe<Array<Maybe<Detection>>>
  meta?: Maybe<PaginationMeta>
}

export type Feed = {
  __typename?: 'Feed'
  id: Scalars['ID']['output']
  introHtml: Scalars['String']['output']
  latLng: Scalars['Json']['output']
  mapUrl: Scalars['String']['output']
  name: Scalars['String']['output']
  nodeName: Scalars['String']['output']
  slug: Scalars['String']['output']
  thumbUrl: Scalars['String']['output']
}

/** Pagination options */
export type Pagination = {
  page: Scalars['Int']['input']
  pageSize: Scalars['Int']['input']
}

/** Pagination results via scrivener */
export type PaginationMeta = {
  __typename?: 'PaginationMeta'
  currentPage: Scalars['Int']['output']
  nextPage?: Maybe<Scalars['Int']['output']>
  previousPage?: Maybe<Scalars['Int']['output']>
  totalEntries: Scalars['Int']['output']
  totalPages: Scalars['Int']['output']
}

export type RootMutationType = {
  __typename?: 'RootMutationType'
  /** Log in */
  login?: Maybe<User>
  /** Log out */
  logout?: Maybe<User>
  /** Create user */
  signup?: Maybe<User>
  /** Submit an orca sound detection */
  submitDetection?: Maybe<DetectionWithLockout>
  /** Update user details */
  updateUser?: Maybe<User>
}

export type RootMutationTypeLoginArgs = {
  email: Scalars['String']['input']
  password: Scalars['String']['input']
}

export type RootMutationTypeLogoutArgs = {
  id?: InputMaybe<Scalars['ID']['input']>
}

export type RootMutationTypeSignupArgs = {
  email: Scalars['String']['input']
  firstName?: InputMaybe<Scalars['String']['input']>
  lastName?: InputMaybe<Scalars['String']['input']>
  password: Scalars['String']['input']
}

export type RootMutationTypeSubmitDetectionArgs = {
  description?: InputMaybe<Scalars['String']['input']>
  feedId?: InputMaybe<Scalars['ID']['input']>
  listenerCount?: InputMaybe<Scalars['Int']['input']>
  playerOffset?: InputMaybe<Scalars['Decimal']['input']>
  playlistTimestamp?: InputMaybe<Scalars['String']['input']>
}

export type RootMutationTypeUpdateUserArgs = {
  admin?: InputMaybe<Scalars['Boolean']['input']>
  id?: InputMaybe<Scalars['ID']['input']>
}

export type RootQueryType = {
  __typename?: 'RootQueryType'
  /** List candidates, paginated */
  candidates?: Maybe<Candidates>
  /** Get logged in user */
  currentUser?: Maybe<User>
  /** List detections, paginated */
  detections?: Maybe<Detections>
  /** List detections */
  detectionsAll?: Maybe<Array<Maybe<Detection>>>
  /** Get a feed */
  feed: Feed
  /** Get a list of feeds */
  feeds: Array<Feed>
  /** List users, paginated */
  users?: Maybe<Users>
}

export type RootQueryTypeCandidatesArgs = {
  pagination?: InputMaybe<Pagination>
}

export type RootQueryTypeDetectionsArgs = {
  pagination?: InputMaybe<Pagination>
}

export type RootQueryTypeFeedArgs = {
  slug?: InputMaybe<Scalars['String']['input']>
}

export type RootQueryTypeUsersArgs = {
  pagination?: InputMaybe<Pagination>
}

/** A user */
export type User = {
  __typename?: 'User'
  admin?: Maybe<Scalars['Boolean']['output']>
  authToken?: Maybe<Scalars['String']['output']>
  email?: Maybe<Scalars['String']['output']>
  firstName?: Maybe<Scalars['String']['output']>
  id?: Maybe<Scalars['ID']['output']>
  lastName?: Maybe<Scalars['String']['output']>
}

/** Pagination version of users */
export type Users = {
  __typename?: 'Users'
  entries?: Maybe<Array<Maybe<User>>>
  meta?: Maybe<PaginationMeta>
}

export type SubmitDetectionMutationVariables = Exact<{
  feedId: Scalars['ID']['input']
  playlistTimestamp: Scalars['String']['input']
  playerOffset: Scalars['Decimal']['input']
  description: Scalars['String']['input']
  listenerCount?: InputMaybe<Scalars['Int']['input']>
}>

export type SubmitDetectionMutation = {
  __typename?: 'RootMutationType'
  submitDetection?: {
    __typename?: 'DetectionWithLockout'
    lockoutInitial?: number | null
    lockoutRemaining?: number | null
    detection?: { __typename?: 'Detection'; id?: string | null } | null
  } | null
}

export type FeedQueryVariables = Exact<{
  slug: Scalars['String']['input']
}>

export type FeedQuery = {
  __typename?: 'RootQueryType'
  feed: {
    __typename?: 'Feed'
    id: string
    name: string
    slug: string
    nodeName: string
    latLng: any
    introHtml: string
    thumbUrl: string
    mapUrl: string
  }
}

export type FeedsQueryVariables = Exact<{ [key: string]: never }>

export type FeedsQuery = {
  __typename?: 'RootQueryType'
  feeds: Array<{
    __typename?: 'Feed'
    id: string
    name: string
    slug: string
    nodeName: string
    latLng: any
    thumbUrl: string
    mapUrl: string
  }>
}

export const SubmitDetectionDocument = `
    mutation submitDetection($feedId: ID!, $playlistTimestamp: String!, $playerOffset: Decimal!, $description: String!, $listenerCount: Int) {
  submitDetection(
    feedId: $feedId
    playlistTimestamp: $playlistTimestamp
    playerOffset: $playerOffset
    listenerCount: $listenerCount
    description: $description
  ) {
    detection {
      id
    }
    lockoutInitial
    lockoutRemaining
  }
}
    `
export const useSubmitDetectionMutation = <
  TError = unknown,
  TContext = unknown
>(
  options?: UseMutationOptions<
    SubmitDetectionMutation,
    TError,
    SubmitDetectionMutationVariables,
    TContext
  >
) =>
  useMutation<
    SubmitDetectionMutation,
    TError,
    SubmitDetectionMutationVariables,
    TContext
  >(
    ['submitDetection'],
    (variables?: SubmitDetectionMutationVariables) =>
      fetcher<SubmitDetectionMutation, SubmitDetectionMutationVariables>(
        SubmitDetectionDocument,
        variables
      )(),
    options
  )
useSubmitDetectionMutation.getKey = () => ['submitDetection']

useSubmitDetectionMutation.fetcher = (
  variables: SubmitDetectionMutationVariables
) =>
  fetcher<SubmitDetectionMutation, SubmitDetectionMutationVariables>(
    SubmitDetectionDocument,
    variables
  )
export const FeedDocument = `
    query feed($slug: String!) {
  feed(slug: $slug) {
    id
    name
    slug
    nodeName
    latLng
    introHtml
    thumbUrl
    mapUrl
  }
}
    `
export const useFeedQuery = <TData = FeedQuery, TError = unknown>(
  variables: FeedQueryVariables,
  options?: UseQueryOptions<FeedQuery, TError, TData>
) =>
  useQuery<FeedQuery, TError, TData>(
    ['feed', variables],
    fetcher<FeedQuery, FeedQueryVariables>(FeedDocument, variables),
    options
  )
useFeedQuery.document = FeedDocument

useFeedQuery.getKey = (variables: FeedQueryVariables) => ['feed', variables]
useFeedQuery.fetcher = (variables: FeedQueryVariables) =>
  fetcher<FeedQuery, FeedQueryVariables>(FeedDocument, variables)
export const FeedsDocument = `
    query feeds {
  feeds {
    id
    name
    slug
    nodeName
    latLng
    thumbUrl
    mapUrl
  }
}
    `
export const useFeedsQuery = <TData = FeedsQuery, TError = unknown>(
  variables?: FeedsQueryVariables,
  options?: UseQueryOptions<FeedsQuery, TError, TData>
) =>
  useQuery<FeedsQuery, TError, TData>(
    variables === undefined ? ['feeds'] : ['feeds', variables],
    fetcher<FeedsQuery, FeedsQueryVariables>(FeedsDocument, variables),
    options
  )
useFeedsQuery.document = FeedsDocument

useFeedsQuery.getKey = (variables?: FeedsQueryVariables) =>
  variables === undefined ? ['feeds'] : ['feeds', variables]
useFeedsQuery.fetcher = (variables?: FeedsQueryVariables) =>
  fetcher<FeedsQuery, FeedsQueryVariables>(FeedsDocument, variables)
