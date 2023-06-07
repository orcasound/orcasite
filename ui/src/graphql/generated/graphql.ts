/* eslint-disable */
import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core'
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
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: { input: string | number; output: string }
  String: { input: string; output: string }
  Boolean: { input: boolean; output: boolean }
  Int: { input: number; output: number }
  Float: { input: number; output: number }
  /**
   * The `DateTime` scalar type represents a date and time in the UTC
   * timezone. The DateTime appears in a JSON response as an ISO8601 formatted
   * string, including UTC timezone ("Z"). The parsed date and time string will
   * be converted to UTC if there is an offset.
   */
  DateTime: { input: any; output: any }
  /**
   * The `Decimal` scalar type represents signed double-precision fractional
   * values parsed by the `Decimal` library. The Decimal appears in a JSON
   * response as a string to preserve precision.
   */
  Decimal: { input: any; output: any }
  /**
   * The `Json` scalar type represents arbitrary json string data, represented as UTF-8
   * character sequences. The Json type is most often used to represent a free-form
   * human-readable json string.
   */
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
  locationPoint: Scalars['Json']['output']
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

export type FeedsQueryVariables = Exact<{ [key: string]: never }>

export type FeedsQuery = {
  __typename?: 'RootQueryType'
  feeds: Array<{
    __typename?: 'Feed'
    id: string
    name: string
    slug: string
    nodeName: string
    locationPoint: any
    thumbUrl: string
    mapUrl: string
  }>
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
    locationPoint: any
    introHtml: string
    thumbUrl: string
    mapUrl: string
  }
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

export const FeedsDocument = {
  kind: 'Document',
  definitions: [
    {
      kind: 'OperationDefinition',
      operation: 'query',
      name: { kind: 'Name', value: 'feeds' },
      selectionSet: {
        kind: 'SelectionSet',
        selections: [
          {
            kind: 'Field',
            name: { kind: 'Name', value: 'feeds' },
            selectionSet: {
              kind: 'SelectionSet',
              selections: [
                { kind: 'Field', name: { kind: 'Name', value: 'id' } },
                { kind: 'Field', name: { kind: 'Name', value: 'name' } },
                { kind: 'Field', name: { kind: 'Name', value: 'slug' } },
                { kind: 'Field', name: { kind: 'Name', value: 'nodeName' } },
                {
                  kind: 'Field',
                  name: { kind: 'Name', value: 'locationPoint' },
                },
                { kind: 'Field', name: { kind: 'Name', value: 'thumbUrl' } },
                { kind: 'Field', name: { kind: 'Name', value: 'mapUrl' } },
              ],
            },
          },
        ],
      },
    },
  ],
} as unknown as DocumentNode<FeedsQuery, FeedsQueryVariables>
export const FeedDocument = {
  kind: 'Document',
  definitions: [
    {
      kind: 'OperationDefinition',
      operation: 'query',
      name: { kind: 'Name', value: 'feed' },
      variableDefinitions: [
        {
          kind: 'VariableDefinition',
          variable: { kind: 'Variable', name: { kind: 'Name', value: 'slug' } },
          type: {
            kind: 'NonNullType',
            type: {
              kind: 'NamedType',
              name: { kind: 'Name', value: 'String' },
            },
          },
        },
      ],
      selectionSet: {
        kind: 'SelectionSet',
        selections: [
          {
            kind: 'Field',
            name: { kind: 'Name', value: 'feed' },
            arguments: [
              {
                kind: 'Argument',
                name: { kind: 'Name', value: 'slug' },
                value: {
                  kind: 'Variable',
                  name: { kind: 'Name', value: 'slug' },
                },
              },
            ],
            selectionSet: {
              kind: 'SelectionSet',
              selections: [
                { kind: 'Field', name: { kind: 'Name', value: 'id' } },
                { kind: 'Field', name: { kind: 'Name', value: 'name' } },
                { kind: 'Field', name: { kind: 'Name', value: 'slug' } },
                { kind: 'Field', name: { kind: 'Name', value: 'nodeName' } },
                {
                  kind: 'Field',
                  name: { kind: 'Name', value: 'locationPoint' },
                },
                { kind: 'Field', name: { kind: 'Name', value: 'introHtml' } },
                { kind: 'Field', name: { kind: 'Name', value: 'thumbUrl' } },
                { kind: 'Field', name: { kind: 'Name', value: 'mapUrl' } },
              ],
            },
          },
        ],
      },
    },
  ],
} as unknown as DocumentNode<FeedQuery, FeedQueryVariables>
export const SubmitDetectionDocument = {
  kind: 'Document',
  definitions: [
    {
      kind: 'OperationDefinition',
      operation: 'mutation',
      name: { kind: 'Name', value: 'submitDetection' },
      variableDefinitions: [
        {
          kind: 'VariableDefinition',
          variable: {
            kind: 'Variable',
            name: { kind: 'Name', value: 'feedId' },
          },
          type: {
            kind: 'NonNullType',
            type: { kind: 'NamedType', name: { kind: 'Name', value: 'ID' } },
          },
        },
        {
          kind: 'VariableDefinition',
          variable: {
            kind: 'Variable',
            name: { kind: 'Name', value: 'playlistTimestamp' },
          },
          type: {
            kind: 'NonNullType',
            type: {
              kind: 'NamedType',
              name: { kind: 'Name', value: 'String' },
            },
          },
        },
        {
          kind: 'VariableDefinition',
          variable: {
            kind: 'Variable',
            name: { kind: 'Name', value: 'playerOffset' },
          },
          type: {
            kind: 'NonNullType',
            type: {
              kind: 'NamedType',
              name: { kind: 'Name', value: 'Decimal' },
            },
          },
        },
        {
          kind: 'VariableDefinition',
          variable: {
            kind: 'Variable',
            name: { kind: 'Name', value: 'description' },
          },
          type: {
            kind: 'NonNullType',
            type: {
              kind: 'NamedType',
              name: { kind: 'Name', value: 'String' },
            },
          },
        },
        {
          kind: 'VariableDefinition',
          variable: {
            kind: 'Variable',
            name: { kind: 'Name', value: 'listenerCount' },
          },
          type: { kind: 'NamedType', name: { kind: 'Name', value: 'Int' } },
        },
      ],
      selectionSet: {
        kind: 'SelectionSet',
        selections: [
          {
            kind: 'Field',
            name: { kind: 'Name', value: 'submitDetection' },
            arguments: [
              {
                kind: 'Argument',
                name: { kind: 'Name', value: 'feedId' },
                value: {
                  kind: 'Variable',
                  name: { kind: 'Name', value: 'feedId' },
                },
              },
              {
                kind: 'Argument',
                name: { kind: 'Name', value: 'playlistTimestamp' },
                value: {
                  kind: 'Variable',
                  name: { kind: 'Name', value: 'playlistTimestamp' },
                },
              },
              {
                kind: 'Argument',
                name: { kind: 'Name', value: 'playerOffset' },
                value: {
                  kind: 'Variable',
                  name: { kind: 'Name', value: 'playerOffset' },
                },
              },
              {
                kind: 'Argument',
                name: { kind: 'Name', value: 'listenerCount' },
                value: {
                  kind: 'Variable',
                  name: { kind: 'Name', value: 'listenerCount' },
                },
              },
              {
                kind: 'Argument',
                name: { kind: 'Name', value: 'description' },
                value: {
                  kind: 'Variable',
                  name: { kind: 'Name', value: 'description' },
                },
              },
            ],
            selectionSet: {
              kind: 'SelectionSet',
              selections: [
                {
                  kind: 'Field',
                  name: { kind: 'Name', value: 'detection' },
                  selectionSet: {
                    kind: 'SelectionSet',
                    selections: [
                      { kind: 'Field', name: { kind: 'Name', value: 'id' } },
                    ],
                  },
                },
                {
                  kind: 'Field',
                  name: { kind: 'Name', value: 'lockoutInitial' },
                },
                {
                  kind: 'Field',
                  name: { kind: 'Name', value: 'lockoutRemaining' },
                },
              ],
            },
          },
        ],
      },
    },
  ],
} as unknown as DocumentNode<
  SubmitDetectionMutation,
  SubmitDetectionMutationVariables
>
