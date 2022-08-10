import { GraphQLClient } from 'graphql-request'
import * as Dom from 'graphql-request/dist/types.dom'
import gql from 'graphql-tag'
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
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string
  String: string
  Boolean: boolean
  Int: number
  Float: number
  DateTime: any
  Decimal: any
  Json: any
}

export type Candidate = {
  __typename?: 'Candidate'
  detectionCount?: Maybe<Scalars['Int']>
  detections?: Maybe<Array<Maybe<Detection>>>
  feed?: Maybe<Feed>
  id?: Maybe<Scalars['ID']>
  maxTime?: Maybe<Scalars['DateTime']>
  minTime?: Maybe<Scalars['DateTime']>
}

/** Pagination version of candidates */
export type Candidates = {
  __typename?: 'Candidates'
  entries?: Maybe<Array<Maybe<Candidate>>>
  meta?: Maybe<PaginationMeta>
}

export type Detection = {
  __typename?: 'Detection'
  description?: Maybe<Scalars['String']>
  feed?: Maybe<Feed>
  id?: Maybe<Scalars['ID']>
  listenerCount?: Maybe<Scalars['Int']>
  playerOffset?: Maybe<Scalars['Decimal']>
  playlistTimestamp?: Maybe<Scalars['Int']>
  sourceIp?: Maybe<Scalars['String']>
  timestamp?: Maybe<Scalars['DateTime']>
}

export type DetectionWithLockout = {
  __typename?: 'DetectionWithLockout'
  detection?: Maybe<Detection>
  lockoutInitial?: Maybe<Scalars['Float']>
  lockoutRemaining?: Maybe<Scalars['Float']>
}

/** Pagination version of detections */
export type Detections = {
  __typename?: 'Detections'
  entries?: Maybe<Array<Maybe<Detection>>>
  meta?: Maybe<PaginationMeta>
}

export type Feed = {
  __typename?: 'Feed'
  id: Scalars['ID']
  introHtml: Scalars['String']
  locationPoint: Scalars['Json']
  mapUrl: Scalars['String']
  name: Scalars['String']
  nodeName: Scalars['String']
  slug: Scalars['String']
  thumbUrl: Scalars['String']
}

/** Pagination options */
export type Pagination = {
  page: Scalars['Int']
  pageSize: Scalars['Int']
}

/** Pagination results via scrivener */
export type PaginationMeta = {
  __typename?: 'PaginationMeta'
  currentPage: Scalars['Int']
  nextPage?: Maybe<Scalars['Int']>
  previousPage?: Maybe<Scalars['Int']>
  totalEntries: Scalars['Int']
  totalPages: Scalars['Int']
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
  email: Scalars['String']
  password: Scalars['String']
}

export type RootMutationTypeSignupArgs = {
  email: Scalars['String']
  firstName?: InputMaybe<Scalars['String']>
  lastName?: InputMaybe<Scalars['String']>
  password: Scalars['String']
}

export type RootMutationTypeSubmitDetectionArgs = {
  description?: InputMaybe<Scalars['String']>
  feedId?: InputMaybe<Scalars['ID']>
  listenerCount?: InputMaybe<Scalars['Int']>
  playerOffset?: InputMaybe<Scalars['Decimal']>
  playlistTimestamp?: InputMaybe<Scalars['String']>
}

export type RootMutationTypeUpdateUserArgs = {
  admin?: InputMaybe<Scalars['Boolean']>
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
  slug?: InputMaybe<Scalars['String']>
}

export type RootQueryTypeUsersArgs = {
  pagination?: InputMaybe<Pagination>
}

/** A user */
export type User = {
  __typename?: 'User'
  admin?: Maybe<Scalars['Boolean']>
  authToken?: Maybe<Scalars['String']>
  email?: Maybe<Scalars['String']>
  firstName?: Maybe<Scalars['String']>
  id?: Maybe<Scalars['ID']>
  lastName?: Maybe<Scalars['String']>
}

/** Pagination version of users */
export type Users = {
  __typename?: 'Users'
  entries?: Maybe<Array<Maybe<User>>>
  meta?: Maybe<PaginationMeta>
}

export type FeedQueryVariables = Exact<{
  slug: Scalars['String']
}>

export type FeedQuery = {
  __typename?: 'RootQueryType'
  feed: {
    __typename?: 'Feed'
    id: string
    name: string
    slug: string
    nodeName: string
    locationPoint: any
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
    thumbUrl: string
    mapUrl: string
  }>
}

export const FeedDocument = gql`
  query feed($slug: String!) {
    feed(slug: $slug) {
      id
      name
      slug
      nodeName
      locationPoint
      introHtml
      thumbUrl
      mapUrl
    }
  }
`
export const FeedsDocument = gql`
  query feeds {
    feeds {
      id
      name
      slug
      nodeName
      thumbUrl
      mapUrl
    }
  }
`

export type SdkFunctionWrapper = <T>(
  action: (requestHeaders?: Record<string, string>) => Promise<T>,
  operationName: string,
  operationType?: string
) => Promise<T>

const defaultWrapper: SdkFunctionWrapper = (
  action,
  _operationName,
  _operationType
) => action()

export function getSdk(
  client: GraphQLClient,
  withWrapper: SdkFunctionWrapper = defaultWrapper
) {
  return {
    feed(
      variables: FeedQueryVariables,
      requestHeaders?: Dom.RequestInit['headers']
    ): Promise<FeedQuery> {
      return withWrapper(
        (wrappedRequestHeaders) =>
          client.request<FeedQuery>(FeedDocument, variables, {
            ...requestHeaders,
            ...wrappedRequestHeaders,
          }),
        'feed',
        'query'
      )
    },
    feeds(
      variables?: FeedsQueryVariables,
      requestHeaders?: Dom.RequestInit['headers']
    ): Promise<FeedsQuery> {
      return withWrapper(
        (wrappedRequestHeaders) =>
          client.request<FeedsQuery>(FeedsDocument, variables, {
            ...requestHeaders,
            ...wrappedRequestHeaders,
          }),
        'feeds',
        'query'
      )
    },
  }
}
export type Sdk = ReturnType<typeof getSdk>
