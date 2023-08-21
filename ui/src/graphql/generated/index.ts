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
  DateTime: { input: any; output: any };
  Decimal: { input: any; output: any };
  Json: { input: any; output: any };
};

export type Candidate = {
  __typename?: "Candidate";
  detectionCount?: Maybe<Scalars["Int"]["output"]>;
  detections: Array<Detection>;
  feed?: Maybe<Feed>;
  id: Scalars["ID"]["output"];
  maxTime?: Maybe<Scalars["DateTime"]["output"]>;
  minTime?: Maybe<Scalars["DateTime"]["output"]>;
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
  or?: InputMaybe<Array<CandidateFilterInput>>;
};

export type CandidateFilterMaxTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type CandidateFilterMinTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export enum CandidateSortField {
  DetectionCount = "DETECTION_COUNT",
  Id = "ID",
  MaxTime = "MAX_TIME",
  MinTime = "MIN_TIME",
}

export type CandidateSortInput = {
  field: CandidateSortField;
  order?: InputMaybe<SortOrder>;
};

export type Detection = {
  __typename?: "Detection";
  candidate?: Maybe<Candidate>;
  description?: Maybe<Scalars["String"]["output"]>;
  feed?: Maybe<Feed>;
  id: Scalars["ID"]["output"];
  listenerCount?: Maybe<Scalars["Int"]["output"]>;
  playerOffset?: Maybe<Scalars["Decimal"]["output"]>;
  playlistTimestamp?: Maybe<Scalars["Int"]["output"]>;
  sourceIp?: Maybe<Scalars["String"]["output"]>;
  timestamp?: Maybe<Scalars["DateTime"]["output"]>;
  uuid?: Maybe<Scalars["String"]["output"]>;
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
  description?: InputMaybe<DetectionFilterDescription>;
  feed?: InputMaybe<FeedFilterInput>;
  id?: InputMaybe<DetectionFilterId>;
  listenerCount?: InputMaybe<DetectionFilterListenerCount>;
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
  in?: InputMaybe<Array<InputMaybe<Scalars["Decimal"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type DetectionFilterPlaylistTimestamp = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
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
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export enum DetectionSortField {
  Description = "DESCRIPTION",
  Id = "ID",
  ListenerCount = "LISTENER_COUNT",
  PlayerOffset = "PLAYER_OFFSET",
  PlaylistTimestamp = "PLAYLIST_TIMESTAMP",
  SourceIp = "SOURCE_IP",
  Timestamp = "TIMESTAMP",
}

export type DetectionSortInput = {
  field: DetectionSortField;
  order?: InputMaybe<SortOrder>;
};

export type Feed = {
  __typename?: "Feed";
  id: Scalars["ID"]["output"];
  introHtml?: Maybe<Scalars["String"]["output"]>;
  latLng?: Maybe<LatLng>;
  latLngString?: Maybe<Scalars["String"]["output"]>;
  locationPoint: Scalars["Json"]["output"];
  mapUrl?: Maybe<Scalars["String"]["output"]>;
  name: Scalars["String"]["output"];
  nodeName: Scalars["String"]["output"];
  slug: Scalars["String"]["output"];
  thumbUrl?: Maybe<Scalars["String"]["output"]>;
};

export type FeedFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterInput = {
  and?: InputMaybe<Array<FeedFilterInput>>;
  id?: InputMaybe<FeedFilterId>;
  introHtml?: InputMaybe<FeedFilterIntroHtml>;
  locationPoint?: InputMaybe<FeedFilterLocationPoint>;
  name?: InputMaybe<FeedFilterName>;
  nodeName?: InputMaybe<FeedFilterNodeName>;
  or?: InputMaybe<Array<FeedFilterInput>>;
  slug?: InputMaybe<FeedFilterSlug>;
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

export enum FeedSortField {
  Id = "ID",
  IntroHtml = "INTRO_HTML",
  LocationPoint = "LOCATION_POINT",
  Name = "NAME",
  NodeName = "NODE_NAME",
  Slug = "SLUG",
}

export type FeedSortInput = {
  field: FeedSortField;
  order?: InputMaybe<SortOrder>;
};

export type LatLng = {
  __typename?: "LatLng";
  lat?: Maybe<Scalars["Float"]["output"]>;
  lng?: Maybe<Scalars["Float"]["output"]>;
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

export type RootMutationType = {
  __typename?: "RootMutationType";
  submitDetection?: Maybe<SubmitDetectionResult>;
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
  feed?: Maybe<Feed>;
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

export enum SortOrder {
  Asc = "ASC",
  Desc = "DESC",
}

export type SubmitDetectionInput = {
  description: Scalars["String"]["input"];
  feedId: Scalars["String"]["input"];
  listenerCount?: InputMaybe<Scalars["Int"]["input"]>;
  playerOffset: Scalars["Decimal"]["input"];
  playlistTimestamp: Scalars["Int"]["input"];
};

/** The result of the :submit_detection mutation */
export type SubmitDetectionResult = {
  __typename?: "SubmitDetectionResult";
  /** Any errors generated, if the mutation failed */
  errors?: Maybe<Array<Maybe<MutationError>>>;
  /** The successful result of the mutation */
  result?: Maybe<Detection>;
};

export type SubmitDetectionMutationVariables = Exact<{
  feedId: Scalars["String"]["input"];
  playlistTimestamp: Scalars["Int"]["input"];
  playerOffset: Scalars["Decimal"]["input"];
  description: Scalars["String"]["input"];
  listenerCount?: InputMaybe<Scalars["Int"]["input"]>;
}>;

export type SubmitDetectionMutation = {
  __typename?: "RootMutationType";
  submitDetection?: {
    __typename?: "SubmitDetectionResult";
    result?: { __typename?: "Detection"; id: string } | null;
  } | null;
};

export type FeedQueryVariables = Exact<{
  slug: Scalars["String"]["input"];
}>;

export type FeedQuery = {
  __typename?: "RootQueryType";
  feed?: {
    __typename?: "Feed";
    id: string;
    name: string;
    slug: string;
    nodeName: string;
    introHtml?: string | null;
    thumbUrl?: string | null;
    mapUrl?: string | null;
    latLng?: {
      __typename?: "LatLng";
      lat?: number | null;
      lng?: number | null;
    } | null;
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
    thumbUrl?: string | null;
    mapUrl?: string | null;
    latLng?: {
      __typename?: "LatLng";
      lat?: number | null;
      lng?: number | null;
    } | null;
  }>;
};

export const SubmitDetectionDocument = `
    mutation submitDetection($feedId: String!, $playlistTimestamp: Int!, $playerOffset: Decimal!, $description: String!, $listenerCount: Int) {
  submitDetection(
    input: {feedId: $feedId, playlistTimestamp: $playlistTimestamp, playerOffset: $playerOffset, listenerCount: $listenerCount, description: $description}
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
