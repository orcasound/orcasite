import { endpointUrl, fetchParams } from "@/graphql/client";
import {
  useMutation,
  useQuery,
  UseMutationOptions,
  UseQueryOptions,
} from "@tanstack/react-query";
export type Maybe<T> = T | null;
export type InputMaybe<T> = Maybe<T>;
export type Exact<T extends { [key: string]: unknown }> = {
  [K in keyof T]: T[K];
};
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & {
  [SubKey in K]?: Maybe<T[SubKey]>;
};
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & {
  [SubKey in K]: Maybe<T[SubKey]>;
};
export type MakeEmpty<
  T extends { [key: string]: unknown },
  K extends keyof T,
> = { [_ in K]?: never };
export type Incremental<T> =
  | T
  | {
      [P in keyof T]?: P extends " $fragmentName" | "__typename" ? T[P] : never;
    };

function fetcher<TData, TVariables>(query: string, variables?: TVariables) {
  return async (): Promise<TData> => {
    const res = await fetch(endpointUrl as string, {
      method: "POST",
      ...fetchParams,
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
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: { input: string; output: string };
  String: { input: string; output: string };
  Boolean: { input: boolean; output: boolean };
  Int: { input: number; output: number };
  Float: { input: number; output: number };
  DateTime: { input: Date; output: Date };
  Decimal: { input: number; output: number };
  Json: { input: { [key: string]: any }; output: { [key: string]: any } };
};

export type Candidate = {
  __typename?: "Candidate";
  detectionCount?: Maybe<Scalars["Int"]["output"]>;
  detections: Array<Detection>;
  feed: Feed;
  id: Scalars["ID"]["output"];
  maxTime: Scalars["DateTime"]["output"];
  minTime: Scalars["DateTime"]["output"];
  uuid?: Maybe<Scalars["String"]["output"]>;
};

export type CandidateDetectionsArgs = {
  filter?: InputMaybe<DetectionFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<DetectionSortInput>>>;
};

export type CandidateFilterDetectionCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type CandidateFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type CandidateFilterInput = {
  and?: InputMaybe<Array<CandidateFilterInput>>;
  detectionCount?: InputMaybe<CandidateFilterDetectionCount>;
  detections?: InputMaybe<DetectionFilterInput>;
  feed?: InputMaybe<FeedFilterInput>;
  id?: InputMaybe<CandidateFilterId>;
  maxTime?: InputMaybe<CandidateFilterMaxTime>;
  minTime?: InputMaybe<CandidateFilterMinTime>;
  not?: InputMaybe<Array<CandidateFilterInput>>;
  or?: InputMaybe<Array<CandidateFilterInput>>;
};

export type CandidateFilterMaxTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type CandidateFilterMinTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type CandidateSortField =
  | "DETECTION_COUNT"
  | "ID"
  | "MAX_TIME"
  | "MIN_TIME";

export type CandidateSortInput = {
  field: CandidateSortField;
  order?: InputMaybe<SortOrder>;
};

export type Detection = {
  __typename?: "Detection";
  candidate?: Maybe<Candidate>;
  category?: Maybe<DetectionCategory>;
  description?: Maybe<Scalars["String"]["output"]>;
  feed?: Maybe<Feed>;
  id: Scalars["ID"]["output"];
  listenerCount?: Maybe<Scalars["Int"]["output"]>;
  playerOffset: Scalars["Decimal"]["output"];
  playlistTimestamp: Scalars["Int"]["output"];
  sourceIp?: Maybe<Scalars["String"]["output"]>;
  timestamp: Scalars["DateTime"]["output"];
  uuid?: Maybe<Scalars["String"]["output"]>;
};

export type DetectionCategory = "OTHER" | "VESSEL" | "WHALE";

export type DetectionFilterCategory = {
  eq?: InputMaybe<DetectionCategory>;
  greaterThan?: InputMaybe<DetectionCategory>;
  greaterThanOrEqual?: InputMaybe<DetectionCategory>;
  in?: InputMaybe<Array<InputMaybe<DetectionCategory>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<DetectionCategory>;
  lessThanOrEqual?: InputMaybe<DetectionCategory>;
  notEq?: InputMaybe<DetectionCategory>;
};

export type DetectionFilterDescription = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type DetectionFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type DetectionFilterInput = {
  and?: InputMaybe<Array<DetectionFilterInput>>;
  candidate?: InputMaybe<CandidateFilterInput>;
  category?: InputMaybe<DetectionFilterCategory>;
  description?: InputMaybe<DetectionFilterDescription>;
  feed?: InputMaybe<FeedFilterInput>;
  id?: InputMaybe<DetectionFilterId>;
  listenerCount?: InputMaybe<DetectionFilterListenerCount>;
  not?: InputMaybe<Array<DetectionFilterInput>>;
  or?: InputMaybe<Array<DetectionFilterInput>>;
  playerOffset?: InputMaybe<DetectionFilterPlayerOffset>;
  playlistTimestamp?: InputMaybe<DetectionFilterPlaylistTimestamp>;
  sourceIp?: InputMaybe<DetectionFilterSourceIp>;
  timestamp?: InputMaybe<DetectionFilterTimestamp>;
};

export type DetectionFilterListenerCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type DetectionFilterPlayerOffset = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<Scalars["Decimal"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type DetectionFilterPlaylistTimestamp = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<Scalars["Int"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type DetectionFilterSourceIp = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type DetectionFilterTimestamp = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type DetectionSortField =
  | "CATEGORY"
  | "DESCRIPTION"
  | "ID"
  | "LISTENER_COUNT"
  | "PLAYER_OFFSET"
  | "PLAYLIST_TIMESTAMP"
  | "SOURCE_IP"
  | "TIMESTAMP";

export type DetectionSortInput = {
  field: DetectionSortField;
  order?: InputMaybe<SortOrder>;
};

export type Feed = {
  __typename?: "Feed";
  id: Scalars["ID"]["output"];
  imageUrl?: Maybe<Scalars["String"]["output"]>;
  introHtml?: Maybe<Scalars["String"]["output"]>;
  latLng: LatLng;
  latLngString?: Maybe<Scalars["String"]["output"]>;
  locationPoint: Scalars["Json"]["output"];
  mapUrl?: Maybe<Scalars["String"]["output"]>;
  name: Scalars["String"]["output"];
  nodeName: Scalars["String"]["output"];
  slug: Scalars["String"]["output"];
  thumbUrl?: Maybe<Scalars["String"]["output"]>;
  visible?: Maybe<Scalars["Boolean"]["output"]>;
};

export type FeedFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterImageUrl = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterInput = {
  and?: InputMaybe<Array<FeedFilterInput>>;
  id?: InputMaybe<FeedFilterId>;
  imageUrl?: InputMaybe<FeedFilterImageUrl>;
  introHtml?: InputMaybe<FeedFilterIntroHtml>;
  locationPoint?: InputMaybe<FeedFilterLocationPoint>;
  name?: InputMaybe<FeedFilterName>;
  nodeName?: InputMaybe<FeedFilterNodeName>;
  not?: InputMaybe<Array<FeedFilterInput>>;
  or?: InputMaybe<Array<FeedFilterInput>>;
  slug?: InputMaybe<FeedFilterSlug>;
  visible?: InputMaybe<FeedFilterVisible>;
};

export type FeedFilterIntroHtml = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterLocationPoint = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterNodeName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterSlug = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterVisible = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedSortField =
  | "ID"
  | "IMAGE_URL"
  | "INTRO_HTML"
  | "LOCATION_POINT"
  | "NAME"
  | "NODE_NAME"
  | "SLUG"
  | "VISIBLE";

export type FeedSortInput = {
  field: FeedSortField;
  order?: InputMaybe<SortOrder>;
};

export type LatLng = {
  __typename?: "LatLng";
  lat: Scalars["Float"]["output"];
  lng: Scalars["Float"]["output"];
};

/** An error generated by a failed mutation */
export type MutationError = {
  __typename?: "MutationError";
  /** An error code for the given error */
  code?: Maybe<Scalars["String"]["output"]>;
  /** The field or fields that produced the error */
  fields?: Maybe<Array<Maybe<Scalars["String"]["output"]>>>;
  /** The human readable error message */
  message?: Maybe<Scalars["String"]["output"]>;
  /** A shorter error message, with vars not replaced */
  shortMessage?: Maybe<Scalars["String"]["output"]>;
  /** Replacements for the short message */
  vars?: Maybe<Scalars["Json"]["output"]>;
};

/** A page of :candidate */
export type PageOfCandidate = {
  __typename?: "PageOfCandidate";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<Candidate>>;
};

/** A page of :detection */
export type PageOfDetection = {
  __typename?: "PageOfDetection";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<Detection>>;
};

export type RegisterWithPasswordInput = {
  email: Scalars["String"]["input"];
  password: Scalars["String"]["input"];
  passwordConfirmation: Scalars["String"]["input"];
};

export type RegisterWithPasswordResult = {
  __typename?: "RegisterWithPasswordResult";
  errors?: Maybe<Array<Maybe<MutationError>>>;
  token?: Maybe<Scalars["String"]["output"]>;
  user?: Maybe<User>;
};

export type RootMutationType = {
  __typename?: "RootMutationType";
  registerWithPassword?: Maybe<RegisterWithPasswordResult>;
  signInWithPassword?: Maybe<SignInWithPasswordResult>;
  submitDetection?: Maybe<SubmitDetectionResult>;
};

export type RootMutationTypeRegisterWithPasswordArgs = {
  input: RegisterWithPasswordInput;
};

export type RootMutationTypeSignInWithPasswordArgs = {
  input: SignInWithPasswordInput;
};

export type RootMutationTypeSubmitDetectionArgs = {
  input?: InputMaybe<SubmitDetectionInput>;
};

export type RootQueryType = {
  __typename?: "RootQueryType";
  candidate?: Maybe<Candidate>;
  candidates?: Maybe<PageOfCandidate>;
  detection?: Maybe<Detection>;
  detections?: Maybe<PageOfDetection>;
  feed: Feed;
  feeds: Array<Feed>;
};

export type RootQueryTypeCandidateArgs = {
  id: Scalars["ID"]["input"];
};

export type RootQueryTypeCandidatesArgs = {
  filter?: InputMaybe<CandidateFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<CandidateSortInput>>>;
};

export type RootQueryTypeDetectionArgs = {
  id: Scalars["ID"]["input"];
};

export type RootQueryTypeDetectionsArgs = {
  filter?: InputMaybe<DetectionFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<DetectionSortInput>>>;
};

export type RootQueryTypeFeedArgs = {
  filter?: InputMaybe<FeedFilterInput>;
  slug?: InputMaybe<Scalars["String"]["input"]>;
};

export type RootQueryTypeFeedsArgs = {
  filter?: InputMaybe<FeedFilterInput>;
  sort?: InputMaybe<Array<InputMaybe<FeedSortInput>>>;
};

export type SignInWithPasswordInput = {
  email: Scalars["String"]["input"];
  password: Scalars["String"]["input"];
};

export type SignInWithPasswordResult = {
  __typename?: "SignInWithPasswordResult";
  errors?: Maybe<Array<Maybe<MutationError>>>;
  token?: Maybe<Scalars["String"]["output"]>;
  user?: Maybe<User>;
};

export type SortOrder =
  | "ASC"
  | "ASC_NULLS_FIRST"
  | "ASC_NULLS_LAST"
  | "DESC"
  | "DESC_NULLS_FIRST"
  | "DESC_NULLS_LAST";

export type SubmitDetectionInput = {
  category: DetectionCategory;
  description?: InputMaybe<Scalars["String"]["input"]>;
  feedId: Scalars["String"]["input"];
  listenerCount?: InputMaybe<Scalars["Int"]["input"]>;
  playerOffset: Scalars["Decimal"]["input"];
  playlistTimestamp: Scalars["Int"]["input"];
  sendNotifications?: InputMaybe<Scalars["Boolean"]["input"]>;
};

/** The result of the :submit_detection mutation */
export type SubmitDetectionResult = {
  __typename?: "SubmitDetectionResult";
  /** Any errors generated, if the mutation failed */
  errors?: Maybe<Array<Maybe<MutationError>>>;
  /** The successful result of the mutation */
  result?: Maybe<Detection>;
};

export type User = {
  __typename?: "User";
  admin?: Maybe<Scalars["Boolean"]["output"]>;
  email: Scalars["String"]["output"];
  firstName?: Maybe<Scalars["String"]["output"]>;
  id: Scalars["ID"]["output"];
  lastName?: Maybe<Scalars["String"]["output"]>;
};

export type SignInWithPasswordMutationVariables = Exact<{
  email: Scalars["String"]["input"];
  password: Scalars["String"]["input"];
}>;

export type SignInWithPasswordMutation = {
  __typename?: "RootMutationType";
  signInWithPassword?: {
    __typename?: "SignInWithPasswordResult";
    token?: string | null;
    user?: {
      __typename?: "User";
      id: string;
      email: string;
      admin?: boolean | null;
      firstName?: string | null;
      lastName?: string | null;
    } | null;
    errors?: Array<{
      __typename?: "MutationError";
      message?: string | null;
      code?: string | null;
      fields?: Array<string | null> | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    } | null> | null;
  } | null;
};

export type SubmitDetectionMutationVariables = Exact<{
  feedId: Scalars["String"]["input"];
  playlistTimestamp: Scalars["Int"]["input"];
  playerOffset: Scalars["Decimal"]["input"];
  description: Scalars["String"]["input"];
  listenerCount?: InputMaybe<Scalars["Int"]["input"]>;
  category: DetectionCategory;
}>;

export type SubmitDetectionMutation = {
  __typename?: "RootMutationType";
  submitDetection?: {
    __typename?: "SubmitDetectionResult";
    result?: { __typename?: "Detection"; id: string } | null;
  } | null;
};

export type CandidateQueryVariables = Exact<{
  id: Scalars["ID"]["input"];
}>;

export type CandidateQuery = {
  __typename?: "RootQueryType";
  candidate?: {
    __typename?: "Candidate";
    id: string;
    minTime: Date;
    maxTime: Date;
    detectionCount?: number | null;
    feed: {
      __typename?: "Feed";
      id: string;
      slug: string;
      name: string;
      nodeName: string;
    };
    detections: Array<{
      __typename?: "Detection";
      id: string;
      category?: DetectionCategory | null;
      description?: string | null;
      listenerCount?: number | null;
      playlistTimestamp: number;
      playerOffset: number;
      timestamp: Date;
    }>;
  } | null;
};

export type FeedQueryVariables = Exact<{
  slug: Scalars["String"]["input"];
}>;

export type FeedQuery = {
  __typename?: "RootQueryType";
  feed: {
    __typename?: "Feed";
    id: string;
    name: string;
    slug: string;
    nodeName: string;
    introHtml?: string | null;
    thumbUrl?: string | null;
    imageUrl?: string | null;
    mapUrl?: string | null;
    latLng: { __typename?: "LatLng"; lat: number; lng: number };
  };
};

export type CandidatesQueryVariables = Exact<{
  filter?: InputMaybe<CandidateFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<
    Array<InputMaybe<CandidateSortInput>> | InputMaybe<CandidateSortInput>
  >;
}>;

export type CandidatesQuery = {
  __typename?: "RootQueryType";
  candidates?: {
    __typename?: "PageOfCandidate";
    count?: number | null;
    hasNextPage: boolean;
    results?: Array<{
      __typename?: "Candidate";
      id: string;
      minTime: Date;
      maxTime: Date;
      detectionCount?: number | null;
      feed: {
        __typename?: "Feed";
        id: string;
        slug: string;
        name: string;
        nodeName: string;
      };
      detections: Array<{
        __typename?: "Detection";
        id: string;
        category?: DetectionCategory | null;
        description?: string | null;
        listenerCount?: number | null;
        playlistTimestamp: number;
        playerOffset: number;
        timestamp: Date;
      }>;
    }> | null;
  } | null;
};

export type FeedsQueryVariables = Exact<{ [key: string]: never }>;

export type FeedsQuery = {
  __typename?: "RootQueryType";
  feeds: Array<{
    __typename?: "Feed";
    id: string;
    name: string;
    slug: string;
    nodeName: string;
    imageUrl?: string | null;
    thumbUrl?: string | null;
    mapUrl?: string | null;
    latLng: { __typename?: "LatLng"; lat: number; lng: number };
  }>;
};

export const SignInWithPasswordDocument = `
    mutation signInWithPassword($email: String!, $password: String!) {
  signInWithPassword(input: {email: $email, password: $password}) {
    token
    user {
      id
      email
      admin
      firstName
      lastName
    }
    errors {
      message
      code
      fields
      shortMessage
      vars
    }
  }
}
    `;
export const useSignInWithPasswordMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    SignInWithPasswordMutation,
    TError,
    SignInWithPasswordMutationVariables,
    TContext
  >,
) =>
  useMutation<
    SignInWithPasswordMutation,
    TError,
    SignInWithPasswordMutationVariables,
    TContext
  >(
    ["signInWithPassword"],
    (variables?: SignInWithPasswordMutationVariables) =>
      fetcher<SignInWithPasswordMutation, SignInWithPasswordMutationVariables>(
        SignInWithPasswordDocument,
        variables,
      )(),
    options,
  );
useSignInWithPasswordMutation.getKey = () => ["signInWithPassword"];

useSignInWithPasswordMutation.fetcher = (
  variables: SignInWithPasswordMutationVariables,
) =>
  fetcher<SignInWithPasswordMutation, SignInWithPasswordMutationVariables>(
    SignInWithPasswordDocument,
    variables,
  );
export const SubmitDetectionDocument = `
    mutation submitDetection($feedId: String!, $playlistTimestamp: Int!, $playerOffset: Decimal!, $description: String!, $listenerCount: Int, $category: DetectionCategory!) {
  submitDetection(
    input: {feedId: $feedId, playlistTimestamp: $playlistTimestamp, playerOffset: $playerOffset, listenerCount: $listenerCount, description: $description, category: $category}
  ) {
    result {
      id
    }
  }
}
    `;
export const useSubmitDetectionMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    SubmitDetectionMutation,
    TError,
    SubmitDetectionMutationVariables,
    TContext
  >,
) =>
  useMutation<
    SubmitDetectionMutation,
    TError,
    SubmitDetectionMutationVariables,
    TContext
  >(
    ["submitDetection"],
    (variables?: SubmitDetectionMutationVariables) =>
      fetcher<SubmitDetectionMutation, SubmitDetectionMutationVariables>(
        SubmitDetectionDocument,
        variables,
      )(),
    options,
  );
useSubmitDetectionMutation.getKey = () => ["submitDetection"];

useSubmitDetectionMutation.fetcher = (
  variables: SubmitDetectionMutationVariables,
) =>
  fetcher<SubmitDetectionMutation, SubmitDetectionMutationVariables>(
    SubmitDetectionDocument,
    variables,
  );
export const CandidateDocument = `
    query candidate($id: ID!) {
  candidate(id: $id) {
    id
    minTime
    maxTime
    detectionCount
    feed {
      id
      slug
      name
      nodeName
    }
    detections {
      id
      category
      description
      listenerCount
      playlistTimestamp
      playerOffset
      timestamp
    }
  }
}
    `;
export const useCandidateQuery = <TData = CandidateQuery, TError = unknown>(
  variables: CandidateQueryVariables,
  options?: UseQueryOptions<CandidateQuery, TError, TData>,
) =>
  useQuery<CandidateQuery, TError, TData>(
    ["candidate", variables],
    fetcher<CandidateQuery, CandidateQueryVariables>(
      CandidateDocument,
      variables,
    ),
    options,
  );
useCandidateQuery.document = CandidateDocument;

useCandidateQuery.getKey = (variables: CandidateQueryVariables) => [
  "candidate",
  variables,
];
useCandidateQuery.fetcher = (variables: CandidateQueryVariables) =>
  fetcher<CandidateQuery, CandidateQueryVariables>(
    CandidateDocument,
    variables,
  );
export const FeedDocument = `
    query feed($slug: String!) {
  feed(slug: $slug) {
    id
    name
    slug
    nodeName
    latLng {
      lat
      lng
    }
    introHtml
    thumbUrl
    imageUrl
    mapUrl
  }
}
    `;
export const useFeedQuery = <TData = FeedQuery, TError = unknown>(
  variables: FeedQueryVariables,
  options?: UseQueryOptions<FeedQuery, TError, TData>,
) =>
  useQuery<FeedQuery, TError, TData>(
    ["feed", variables],
    fetcher<FeedQuery, FeedQueryVariables>(FeedDocument, variables),
    options,
  );
useFeedQuery.document = FeedDocument;

useFeedQuery.getKey = (variables: FeedQueryVariables) => ["feed", variables];
useFeedQuery.fetcher = (variables: FeedQueryVariables) =>
  fetcher<FeedQuery, FeedQueryVariables>(FeedDocument, variables);
export const CandidatesDocument = `
    query candidates($filter: CandidateFilterInput, $limit: Int, $offset: Int, $sort: [CandidateSortInput]) {
  candidates(filter: $filter, limit: $limit, offset: $offset, sort: $sort) {
    count
    hasNextPage
    results {
      id
      minTime
      maxTime
      detectionCount
      feed {
        id
        slug
        name
        nodeName
      }
      detections {
        id
        category
        description
        listenerCount
        playlistTimestamp
        playerOffset
        timestamp
      }
    }
  }
}
    `;
export const useCandidatesQuery = <TData = CandidatesQuery, TError = unknown>(
  variables?: CandidatesQueryVariables,
  options?: UseQueryOptions<CandidatesQuery, TError, TData>,
) =>
  useQuery<CandidatesQuery, TError, TData>(
    variables === undefined ? ["candidates"] : ["candidates", variables],
    fetcher<CandidatesQuery, CandidatesQueryVariables>(
      CandidatesDocument,
      variables,
    ),
    options,
  );
useCandidatesQuery.document = CandidatesDocument;

useCandidatesQuery.getKey = (variables?: CandidatesQueryVariables) =>
  variables === undefined ? ["candidates"] : ["candidates", variables];
useCandidatesQuery.fetcher = (variables?: CandidatesQueryVariables) =>
  fetcher<CandidatesQuery, CandidatesQueryVariables>(
    CandidatesDocument,
    variables,
  );
export const FeedsDocument = `
    query feeds {
  feeds {
    id
    name
    slug
    nodeName
    latLng {
      lat
      lng
    }
    imageUrl
    thumbUrl
    mapUrl
  }
}
    `;
export const useFeedsQuery = <TData = FeedsQuery, TError = unknown>(
  variables?: FeedsQueryVariables,
  options?: UseQueryOptions<FeedsQuery, TError, TData>,
) =>
  useQuery<FeedsQuery, TError, TData>(
    variables === undefined ? ["feeds"] : ["feeds", variables],
    fetcher<FeedsQuery, FeedsQueryVariables>(FeedsDocument, variables),
    options,
  );
useFeedsQuery.document = FeedsDocument;

useFeedsQuery.getKey = (variables?: FeedsQueryVariables) =>
  variables === undefined ? ["feeds"] : ["feeds", variables];
useFeedsQuery.fetcher = (variables?: FeedsQueryVariables) =>
  fetcher<FeedsQuery, FeedsQueryVariables>(FeedsDocument, variables);
