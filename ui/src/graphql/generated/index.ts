import {
  useMutation,
  useQuery,
  UseMutationOptions,
  UseQueryOptions,
} from "@tanstack/react-query";
import { fetcher } from "@/graphql/client";
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

export type CancelCandidateNotificationsInput = {
  eventType?: InputMaybe<CandidateEventType>;
};

/** The result of the :cancel_candidate_notifications mutation */
export type CancelCandidateNotificationsResult = {
  __typename?: "CancelCandidateNotificationsResult";
  /** Any errors generated, if the mutation failed */
  errors?: Maybe<Array<Maybe<MutationError>>>;
  /** The successful result of the mutation */
  result?: Maybe<Candidate>;
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
  visible?: Maybe<Scalars["Boolean"]["output"]>;
};

export type CandidateDetectionsArgs = {
  filter?: InputMaybe<DetectionFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<DetectionSortInput>>>;
};

export type CandidateEventType = "CONFIRMED_CANDIDATE" | "NEW_DETECTION";

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
  visible?: InputMaybe<CandidateFilterVisible>;
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

export type CandidateFilterVisible = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type CandidateSortField =
  | "DETECTION_COUNT"
  | "ID"
  | "MAX_TIME"
  | "MIN_TIME"
  | "VISIBLE";

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
  visible?: Maybe<Scalars["Boolean"]["output"]>;
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
  visible?: InputMaybe<DetectionFilterVisible>;
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

export type DetectionFilterVisible = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type DetectionSortField =
  | "CATEGORY"
  | "DESCRIPTION"
  | "ID"
  | "LISTENER_COUNT"
  | "PLAYER_OFFSET"
  | "PLAYLIST_TIMESTAMP"
  | "SOURCE_IP"
  | "TIMESTAMP"
  | "VISIBLE";

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

/**
 * Notification for a specific event type. Once created, all Subscriptions that match this Notification's
 * event type (new detection, confirmed candidate, etc.) will be notified using the Subscription's particular
 * channel settings (email, browser notification, webhooks).
 */
export type Notification = {
  __typename?: "Notification";
  active?: Maybe<Scalars["Boolean"]["output"]>;
  eventType?: Maybe<NotificationEventType>;
  id: Scalars["ID"]["output"];
  meta?: Maybe<Scalars["Json"]["output"]>;
};

export type NotificationEventType = "CONFIRMED_CANDIDATE" | "NEW_DETECTION";

export type NotifyConfirmedCandidateInput = {
  candidateId: Scalars["ID"]["input"];
  /**
   * What primary message subscribers will get (e.g. 'Southern Resident Killer Whales calls
   * and clicks can be heard at Orcasound Lab!')
   */
  message: Scalars["String"]["input"];
};

/** The result of the :notify_confirmed_candidate mutation */
export type NotifyConfirmedCandidateResult = {
  __typename?: "NotifyConfirmedCandidateResult";
  /** Any errors generated, if the mutation failed */
  errors?: Maybe<Array<Maybe<MutationError>>>;
  /** The successful result of the mutation */
  result?: Maybe<Notification>;
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

export type PasswordResetInput = {
  password: Scalars["String"]["input"];
  passwordConfirmation: Scalars["String"]["input"];
  resetToken: Scalars["String"]["input"];
};

export type PasswordResetResult = {
  __typename?: "PasswordResetResult";
  errors?: Maybe<Array<Maybe<MutationError>>>;
  user?: Maybe<User>;
};

export type RegisterWithPasswordInput = {
  email: Scalars["String"]["input"];
  firstName?: InputMaybe<Scalars["String"]["input"]>;
  lastName?: InputMaybe<Scalars["String"]["input"]>;
  /** The proposed password for the user, in plain text. */
  password: Scalars["String"]["input"];
  /** The proposed password for the user (again), in plain text. */
  passwordConfirmation: Scalars["String"]["input"];
};

export type RegisterWithPasswordMetadata = {
  __typename?: "RegisterWithPasswordMetadata";
  /** A JWT which the user can use to authenticate to the API. */
  token: Scalars["String"]["output"];
};

/** The result of the :register_with_password mutation */
export type RegisterWithPasswordResult = {
  __typename?: "RegisterWithPasswordResult";
  /** Any errors generated, if the mutation failed */
  errors?: Maybe<Array<Maybe<MutationError>>>;
  /** Metadata produced by the mutation */
  metadata?: Maybe<RegisterWithPasswordMetadata>;
  /** The successful result of the mutation */
  result?: Maybe<User>;
};

export type RequestPasswordResetInput = {
  email: Scalars["String"]["input"];
};

export type RootMutationType = {
  __typename?: "RootMutationType";
  cancelCandidateNotifications?: Maybe<CancelCandidateNotificationsResult>;
  /** Create a notification for confirmed candidate (i.e. detection group) */
  notifyConfirmedCandidate?: Maybe<NotifyConfirmedCandidateResult>;
  /** Register a new user with a username and password. */
  registerWithPassword?: Maybe<RegisterWithPasswordResult>;
  requestPasswordReset?: Maybe<Scalars["Boolean"]["output"]>;
  resetPassword?: Maybe<PasswordResetResult>;
  setDetectionVisible?: Maybe<SetDetectionVisibleResult>;
  signInWithPassword?: Maybe<SignInWithPasswordResult>;
  signOut?: Maybe<Scalars["Boolean"]["output"]>;
  submitDetection?: Maybe<SubmitDetectionResult>;
};

export type RootMutationTypeCancelCandidateNotificationsArgs = {
  id?: InputMaybe<Scalars["ID"]["input"]>;
  input?: InputMaybe<CancelCandidateNotificationsInput>;
};

export type RootMutationTypeNotifyConfirmedCandidateArgs = {
  input?: InputMaybe<NotifyConfirmedCandidateInput>;
};

export type RootMutationTypeRegisterWithPasswordArgs = {
  input?: InputMaybe<RegisterWithPasswordInput>;
};

export type RootMutationTypeRequestPasswordResetArgs = {
  input: RequestPasswordResetInput;
};

export type RootMutationTypeResetPasswordArgs = {
  input: PasswordResetInput;
};

export type RootMutationTypeSetDetectionVisibleArgs = {
  id?: InputMaybe<Scalars["ID"]["input"]>;
  input?: InputMaybe<SetDetectionVisibleInput>;
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
  currentUser?: Maybe<User>;
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

export type RootQueryTypeCurrentUserArgs = {
  filter?: InputMaybe<UserFilterInput>;
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

export type SetDetectionVisibleInput = {
  visible?: InputMaybe<Scalars["Boolean"]["input"]>;
};

/** The result of the :set_detection_visible mutation */
export type SetDetectionVisibleResult = {
  __typename?: "SetDetectionVisibleResult";
  /** Any errors generated, if the mutation failed */
  errors?: Maybe<Array<Maybe<MutationError>>>;
  /** The successful result of the mutation */
  result?: Maybe<Detection>;
};

export type SignInWithPasswordInput = {
  email: Scalars["String"]["input"];
  password: Scalars["String"]["input"];
};

export type SignInWithPasswordResult = {
  __typename?: "SignInWithPasswordResult";
  errors?: Maybe<Array<Maybe<MutationError>>>;
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
  moderator?: Maybe<Scalars["Boolean"]["output"]>;
};

export type UserFilterAdmin = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type UserFilterEmail = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserFilterFirstName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type UserFilterInput = {
  admin?: InputMaybe<UserFilterAdmin>;
  and?: InputMaybe<Array<UserFilterInput>>;
  email?: InputMaybe<UserFilterEmail>;
  firstName?: InputMaybe<UserFilterFirstName>;
  id?: InputMaybe<UserFilterId>;
  lastName?: InputMaybe<UserFilterLastName>;
  moderator?: InputMaybe<UserFilterModerator>;
  not?: InputMaybe<Array<UserFilterInput>>;
  or?: InputMaybe<Array<UserFilterInput>>;
};

export type UserFilterLastName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserFilterModerator = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type CancelCandidateNotificationsMutationVariables = Exact<{
  candidateId: Scalars["ID"]["input"];
}>;

export type CancelCandidateNotificationsMutation = {
  __typename?: "RootMutationType";
  cancelCandidateNotifications?: {
    __typename?: "CancelCandidateNotificationsResult";
    result?: { __typename?: "Candidate"; id: string } | null;
    errors?: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string | null> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    } | null> | null;
  } | null;
};

export type NotifyConfirmedCandidateMutationVariables = Exact<{
  candidateId: Scalars["ID"]["input"];
  message: Scalars["String"]["input"];
}>;

export type NotifyConfirmedCandidateMutation = {
  __typename?: "RootMutationType";
  notifyConfirmedCandidate?: {
    __typename?: "NotifyConfirmedCandidateResult";
    result?: {
      __typename?: "Notification";
      id: string;
      eventType?: NotificationEventType | null;
      meta?: { [key: string]: any } | null;
      active?: boolean | null;
    } | null;
    errors?: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string | null> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    } | null> | null;
  } | null;
};

export type RegisterWithPasswordMutationVariables = Exact<{
  firstName?: InputMaybe<Scalars["String"]["input"]>;
  lastName?: InputMaybe<Scalars["String"]["input"]>;
  email: Scalars["String"]["input"];
  password: Scalars["String"]["input"];
  passwordConfirmation: Scalars["String"]["input"];
}>;

export type RegisterWithPasswordMutation = {
  __typename?: "RootMutationType";
  registerWithPassword?: {
    __typename?: "RegisterWithPasswordResult";
    result?: {
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

export type RequestPasswordResetMutationVariables = Exact<{
  email: Scalars["String"]["input"];
}>;

export type RequestPasswordResetMutation = {
  __typename?: "RootMutationType";
  requestPasswordReset?: boolean | null;
};

export type ResetPasswordMutationVariables = Exact<{
  password: Scalars["String"]["input"];
  passwordConfirmation: Scalars["String"]["input"];
  resetToken: Scalars["String"]["input"];
}>;

export type ResetPasswordMutation = {
  __typename?: "RootMutationType";
  resetPassword?: {
    __typename?: "PasswordResetResult";
    errors?: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string | null> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    } | null> | null;
    user?: {
      __typename?: "User";
      id: string;
      email: string;
      firstName?: string | null;
      lastName?: string | null;
      admin?: boolean | null;
    } | null;
  } | null;
};

export type SetDetectionVisibleMutationVariables = Exact<{
  id: Scalars["ID"]["input"];
  visible: Scalars["Boolean"]["input"];
}>;

export type SetDetectionVisibleMutation = {
  __typename?: "RootMutationType";
  setDetectionVisible?: {
    __typename?: "SetDetectionVisibleResult";
    result?: {
      __typename?: "Detection";
      id: string;
      visible?: boolean | null;
    } | null;
    errors?: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string | null> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    } | null> | null;
  } | null;
};

export type SignInWithPasswordMutationVariables = Exact<{
  email: Scalars["String"]["input"];
  password: Scalars["String"]["input"];
}>;

export type SignInWithPasswordMutation = {
  __typename?: "RootMutationType";
  signInWithPassword?: {
    __typename?: "SignInWithPasswordResult";
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

export type SignOutMutationVariables = Exact<{ [key: string]: never }>;

export type SignOutMutation = {
  __typename?: "RootMutationType";
  signOut?: boolean | null;
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
    visible?: boolean | null;
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
      visible?: boolean | null;
    }>;
  } | null;
};

export type GetCurrentUserQueryVariables = Exact<{ [key: string]: never }>;

export type GetCurrentUserQuery = {
  __typename?: "RootQueryType";
  currentUser?: {
    __typename?: "User";
    id: string;
    firstName?: string | null;
    lastName?: string | null;
    email: string;
    admin?: boolean | null;
    moderator?: boolean | null;
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
      visible?: boolean | null;
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
        visible?: boolean | null;
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

export const CancelCandidateNotificationsDocument = `
    mutation cancelCandidateNotifications($candidateId: ID!) {
  cancelCandidateNotifications(id: $candidateId) {
    result {
      id
    }
    errors {
      code
      fields
      message
      shortMessage
      vars
    }
  }
}
    `;
export const useCancelCandidateNotificationsMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    CancelCandidateNotificationsMutation,
    TError,
    CancelCandidateNotificationsMutationVariables,
    TContext
  >,
) =>
  useMutation<
    CancelCandidateNotificationsMutation,
    TError,
    CancelCandidateNotificationsMutationVariables,
    TContext
  >(
    ["cancelCandidateNotifications"],
    (variables?: CancelCandidateNotificationsMutationVariables) =>
      fetcher<
        CancelCandidateNotificationsMutation,
        CancelCandidateNotificationsMutationVariables
      >(CancelCandidateNotificationsDocument, variables)(),
    options,
  );
useCancelCandidateNotificationsMutation.getKey = () => [
  "cancelCandidateNotifications",
];

useCancelCandidateNotificationsMutation.fetcher = (
  variables: CancelCandidateNotificationsMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<
    CancelCandidateNotificationsMutation,
    CancelCandidateNotificationsMutationVariables
  >(CancelCandidateNotificationsDocument, variables, options);
export const NotifyConfirmedCandidateDocument = `
    mutation notifyConfirmedCandidate($candidateId: ID!, $message: String!) {
  notifyConfirmedCandidate(input: {candidateId: $candidateId, message: $message}) {
    result {
      id
      eventType
      meta
      active
    }
    errors {
      code
      fields
      message
      shortMessage
      vars
    }
  }
}
    `;
export const useNotifyConfirmedCandidateMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    NotifyConfirmedCandidateMutation,
    TError,
    NotifyConfirmedCandidateMutationVariables,
    TContext
  >,
) =>
  useMutation<
    NotifyConfirmedCandidateMutation,
    TError,
    NotifyConfirmedCandidateMutationVariables,
    TContext
  >(
    ["notifyConfirmedCandidate"],
    (variables?: NotifyConfirmedCandidateMutationVariables) =>
      fetcher<
        NotifyConfirmedCandidateMutation,
        NotifyConfirmedCandidateMutationVariables
      >(NotifyConfirmedCandidateDocument, variables)(),
    options,
  );
useNotifyConfirmedCandidateMutation.getKey = () => ["notifyConfirmedCandidate"];

useNotifyConfirmedCandidateMutation.fetcher = (
  variables: NotifyConfirmedCandidateMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<
    NotifyConfirmedCandidateMutation,
    NotifyConfirmedCandidateMutationVariables
  >(NotifyConfirmedCandidateDocument, variables, options);
export const RegisterWithPasswordDocument = `
    mutation registerWithPassword($firstName: String, $lastName: String, $email: String!, $password: String!, $passwordConfirmation: String!) {
  registerWithPassword(
    input: {email: $email, password: $password, passwordConfirmation: $passwordConfirmation, firstName: $firstName, lastName: $lastName}
  ) {
    result {
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
export const useRegisterWithPasswordMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    RegisterWithPasswordMutation,
    TError,
    RegisterWithPasswordMutationVariables,
    TContext
  >,
) =>
  useMutation<
    RegisterWithPasswordMutation,
    TError,
    RegisterWithPasswordMutationVariables,
    TContext
  >(
    ["registerWithPassword"],
    (variables?: RegisterWithPasswordMutationVariables) =>
      fetcher<
        RegisterWithPasswordMutation,
        RegisterWithPasswordMutationVariables
      >(RegisterWithPasswordDocument, variables)(),
    options,
  );
useRegisterWithPasswordMutation.getKey = () => ["registerWithPassword"];

useRegisterWithPasswordMutation.fetcher = (
  variables: RegisterWithPasswordMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<RegisterWithPasswordMutation, RegisterWithPasswordMutationVariables>(
    RegisterWithPasswordDocument,
    variables,
    options,
  );
export const RequestPasswordResetDocument = `
    mutation requestPasswordReset($email: String!) {
  requestPasswordReset(input: {email: $email})
}
    `;
export const useRequestPasswordResetMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    RequestPasswordResetMutation,
    TError,
    RequestPasswordResetMutationVariables,
    TContext
  >,
) =>
  useMutation<
    RequestPasswordResetMutation,
    TError,
    RequestPasswordResetMutationVariables,
    TContext
  >(
    ["requestPasswordReset"],
    (variables?: RequestPasswordResetMutationVariables) =>
      fetcher<
        RequestPasswordResetMutation,
        RequestPasswordResetMutationVariables
      >(RequestPasswordResetDocument, variables)(),
    options,
  );
useRequestPasswordResetMutation.getKey = () => ["requestPasswordReset"];

useRequestPasswordResetMutation.fetcher = (
  variables: RequestPasswordResetMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<RequestPasswordResetMutation, RequestPasswordResetMutationVariables>(
    RequestPasswordResetDocument,
    variables,
    options,
  );
export const ResetPasswordDocument = `
    mutation resetPassword($password: String!, $passwordConfirmation: String!, $resetToken: String!) {
  resetPassword(
    input: {password: $password, passwordConfirmation: $passwordConfirmation, resetToken: $resetToken}
  ) {
    errors {
      code
      fields
      message
      shortMessage
      vars
    }
    user {
      id
      email
      firstName
      lastName
      admin
    }
  }
}
    `;
export const useResetPasswordMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    ResetPasswordMutation,
    TError,
    ResetPasswordMutationVariables,
    TContext
  >,
) =>
  useMutation<
    ResetPasswordMutation,
    TError,
    ResetPasswordMutationVariables,
    TContext
  >(
    ["resetPassword"],
    (variables?: ResetPasswordMutationVariables) =>
      fetcher<ResetPasswordMutation, ResetPasswordMutationVariables>(
        ResetPasswordDocument,
        variables,
      )(),
    options,
  );
useResetPasswordMutation.getKey = () => ["resetPassword"];

useResetPasswordMutation.fetcher = (
  variables: ResetPasswordMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<ResetPasswordMutation, ResetPasswordMutationVariables>(
    ResetPasswordDocument,
    variables,
    options,
  );
export const SetDetectionVisibleDocument = `
    mutation setDetectionVisible($id: ID!, $visible: Boolean!) {
  setDetectionVisible(id: $id, input: {visible: $visible}) {
    result {
      id
      visible
    }
    errors {
      code
      fields
      message
      shortMessage
      vars
    }
  }
}
    `;
export const useSetDetectionVisibleMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    SetDetectionVisibleMutation,
    TError,
    SetDetectionVisibleMutationVariables,
    TContext
  >,
) =>
  useMutation<
    SetDetectionVisibleMutation,
    TError,
    SetDetectionVisibleMutationVariables,
    TContext
  >(
    ["setDetectionVisible"],
    (variables?: SetDetectionVisibleMutationVariables) =>
      fetcher<
        SetDetectionVisibleMutation,
        SetDetectionVisibleMutationVariables
      >(SetDetectionVisibleDocument, variables)(),
    options,
  );
useSetDetectionVisibleMutation.getKey = () => ["setDetectionVisible"];

useSetDetectionVisibleMutation.fetcher = (
  variables: SetDetectionVisibleMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<SetDetectionVisibleMutation, SetDetectionVisibleMutationVariables>(
    SetDetectionVisibleDocument,
    variables,
    options,
  );
export const SignInWithPasswordDocument = `
    mutation signInWithPassword($email: String!, $password: String!) {
  signInWithPassword(input: {email: $email, password: $password}) {
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
  options?: RequestInit["headers"],
) =>
  fetcher<SignInWithPasswordMutation, SignInWithPasswordMutationVariables>(
    SignInWithPasswordDocument,
    variables,
    options,
  );
export const SignOutDocument = `
    mutation signOut {
  signOut
}
    `;
export const useSignOutMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    SignOutMutation,
    TError,
    SignOutMutationVariables,
    TContext
  >,
) =>
  useMutation<SignOutMutation, TError, SignOutMutationVariables, TContext>(
    ["signOut"],
    (variables?: SignOutMutationVariables) =>
      fetcher<SignOutMutation, SignOutMutationVariables>(
        SignOutDocument,
        variables,
      )(),
    options,
  );
useSignOutMutation.getKey = () => ["signOut"];

useSignOutMutation.fetcher = (
  variables?: SignOutMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<SignOutMutation, SignOutMutationVariables>(
    SignOutDocument,
    variables,
    options,
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
  options?: RequestInit["headers"],
) =>
  fetcher<SubmitDetectionMutation, SubmitDetectionMutationVariables>(
    SubmitDetectionDocument,
    variables,
    options,
  );
export const CandidateDocument = `
    query candidate($id: ID!) {
  candidate(id: $id) {
    id
    minTime
    maxTime
    detectionCount
    visible
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
      visible
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
useCandidateQuery.fetcher = (
  variables: CandidateQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<CandidateQuery, CandidateQueryVariables>(
    CandidateDocument,
    variables,
    options,
  );
export const GetCurrentUserDocument = `
    query getCurrentUser {
  currentUser {
    id
    firstName
    lastName
    email
    admin
    moderator
  }
}
    `;
export const useGetCurrentUserQuery = <
  TData = GetCurrentUserQuery,
  TError = unknown,
>(
  variables?: GetCurrentUserQueryVariables,
  options?: UseQueryOptions<GetCurrentUserQuery, TError, TData>,
) =>
  useQuery<GetCurrentUserQuery, TError, TData>(
    variables === undefined
      ? ["getCurrentUser"]
      : ["getCurrentUser", variables],
    fetcher<GetCurrentUserQuery, GetCurrentUserQueryVariables>(
      GetCurrentUserDocument,
      variables,
    ),
    options,
  );
useGetCurrentUserQuery.document = GetCurrentUserDocument;

useGetCurrentUserQuery.getKey = (variables?: GetCurrentUserQueryVariables) =>
  variables === undefined ? ["getCurrentUser"] : ["getCurrentUser", variables];
useGetCurrentUserQuery.fetcher = (
  variables?: GetCurrentUserQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<GetCurrentUserQuery, GetCurrentUserQueryVariables>(
    GetCurrentUserDocument,
    variables,
    options,
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
useFeedQuery.fetcher = (
  variables: FeedQueryVariables,
  options?: RequestInit["headers"],
) => fetcher<FeedQuery, FeedQueryVariables>(FeedDocument, variables, options);
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
      visible
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
        visible
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
useCandidatesQuery.fetcher = (
  variables?: CandidatesQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<CandidatesQuery, CandidatesQueryVariables>(
    CandidatesDocument,
    variables,
    options,
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
useFeedsQuery.fetcher = (
  variables?: FeedsQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<FeedsQuery, FeedsQueryVariables>(FeedsDocument, variables, options);
