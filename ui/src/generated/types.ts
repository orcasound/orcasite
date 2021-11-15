export type Maybe<T> = T | null
export type Exact<T extends { [key: string]: unknown }> = {
  [K in keyof T]: T[K]
}
export type MakeOptional<T, K extends keyof T> = Omit<T, K> &
  { [SubKey in K]?: Maybe<T[SubKey]> }
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> &
  { [SubKey in K]: Maybe<T[SubKey]> }
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string
  String: string
  Boolean: boolean
  Int: number
  Float: number
  /**
   * The `DateTime` scalar type represents a date and time in the UTC
   * timezone. The DateTime appears in a JSON response as an ISO8601 formatted
   * string, including UTC timezone ("Z"). The parsed date and time string will
   * be converted to UTC and any UTC offset other than 0 will be rejected.
   */
  DateTime: any
  /**
   * The `Decimal` scalar type represents signed double-precision fractional
   * values parsed by the `Decimal` library.  The Decimal appears in a JSON
   * response as a string to preserve precision.
   */
  Decimal: any
  /**
   * The `Json` scalar type represents arbitrary json string data, represented as UTF-8
   * character sequences. The Json type is most often used to represent a free-form
   * human-readable json string.
   */
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
  firstName?: Maybe<Scalars['String']>
  lastName?: Maybe<Scalars['String']>
  password: Scalars['String']
}

export type RootMutationTypeSubmitDetectionArgs = {
  description?: Maybe<Scalars['String']>
  feedId?: Maybe<Scalars['ID']>
  listenerCount?: Maybe<Scalars['Int']>
  playerOffset?: Maybe<Scalars['Decimal']>
  playlistTimestamp?: Maybe<Scalars['String']>
}

export type RootMutationTypeUpdateUserArgs = {
  admin?: Maybe<Scalars['Boolean']>
}

export type RootQueryType = {
  __typename?: 'RootQueryType'
  /** List candidates, paginated */
  candidates?: Maybe<Candidates>
  /** Get logged in user */
  currentUser?: Maybe<User>
  /** List detections */
  detections?: Maybe<Array<Maybe<Detection>>>
  /** Get a feed */
  feed: Feed
  /** Get a list of feeds */
  feeds: Array<Feed>
  /** List users, paginated */
  users?: Maybe<Users>
}

export type RootQueryTypeCandidatesArgs = {
  pagination?: Maybe<Pagination>
}

export type RootQueryTypeFeedArgs = {
  slug?: Maybe<Scalars['String']>
}

export type RootQueryTypeUsersArgs = {
  pagination?: Maybe<Pagination>
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

/** Pagination version of candidates */
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
    thumbUrl: string
    mapUrl: string
  }>
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
