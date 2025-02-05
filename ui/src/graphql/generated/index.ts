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

export type AudioCategory = "ANTHROPHONY" | "BIOPHONY" | "GEOPHONY";

export type AudioImage = {
  __typename?: "AudioImage";
  audioImageFeedSegments: Array<AudioImageFeedSegment>;
  bucket?: Maybe<Scalars["String"]["output"]>;
  bucketRegion?: Maybe<Scalars["String"]["output"]>;
  endTime: Scalars["DateTime"]["output"];
  feed: Feed;
  feedId: Scalars["ID"]["output"];
  feedSegments: Array<FeedSegment>;
  id: Scalars["ID"]["output"];
  imageSize?: Maybe<Scalars["Int"]["output"]>;
  imageType?: Maybe<ImageType>;
  objectPath?: Maybe<Scalars["String"]["output"]>;
  /** Parameters used for generating the image (e.g. n_fft for spectrograms, etc) */
  parameters?: Maybe<Scalars["Json"]["output"]>;
  startTime: Scalars["DateTime"]["output"];
  status: Scalars["String"]["output"];
};

export type AudioImageAudioImageFeedSegmentsArgs = {
  filter?: InputMaybe<AudioImageFeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageFeedSegmentSortInput>>>;
};

export type AudioImageFeedSegmentsArgs = {
  filter?: InputMaybe<FeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedSegmentSortInput>>>;
};

export type AudioImageFeedSegment = {
  __typename?: "AudioImageFeedSegment";
  audioImage?: Maybe<AudioImage>;
  audioImageId?: Maybe<Scalars["ID"]["output"]>;
  feedSegment?: Maybe<FeedSegment>;
  feedSegmentId?: Maybe<Scalars["ID"]["output"]>;
  id: Scalars["ID"]["output"];
};

export type AudioImageFeedSegmentFilterAudioImageId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["ID"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type AudioImageFeedSegmentFilterFeedSegmentId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type AudioImageFeedSegmentFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type AudioImageFeedSegmentFilterInput = {
  and?: InputMaybe<Array<AudioImageFeedSegmentFilterInput>>;
  audioImage?: InputMaybe<AudioImageFilterInput>;
  audioImageId?: InputMaybe<AudioImageFeedSegmentFilterAudioImageId>;
  feedSegment?: InputMaybe<FeedSegmentFilterInput>;
  feedSegmentId?: InputMaybe<AudioImageFeedSegmentFilterFeedSegmentId>;
  id?: InputMaybe<AudioImageFeedSegmentFilterId>;
  not?: InputMaybe<Array<AudioImageFeedSegmentFilterInput>>;
  or?: InputMaybe<Array<AudioImageFeedSegmentFilterInput>>;
};

export type AudioImageFeedSegmentSortField =
  | "AUDIO_IMAGE_ID"
  | "FEED_SEGMENT_ID"
  | "ID";

export type AudioImageFeedSegmentSortInput = {
  field: AudioImageFeedSegmentSortField;
  order?: InputMaybe<SortOrder>;
};

export type AudioImageFilterBucket = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type AudioImageFilterBucketRegion = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type AudioImageFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type AudioImageFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type AudioImageFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type AudioImageFilterImageSize = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type AudioImageFilterImageType = {
  eq?: InputMaybe<ImageType>;
  greaterThan?: InputMaybe<ImageType>;
  greaterThanOrEqual?: InputMaybe<ImageType>;
  in?: InputMaybe<Array<InputMaybe<ImageType>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<ImageType>;
  lessThanOrEqual?: InputMaybe<ImageType>;
  notEq?: InputMaybe<ImageType>;
};

export type AudioImageFilterInput = {
  and?: InputMaybe<Array<AudioImageFilterInput>>;
  audioImageFeedSegments?: InputMaybe<AudioImageFeedSegmentFilterInput>;
  bucket?: InputMaybe<AudioImageFilterBucket>;
  bucketRegion?: InputMaybe<AudioImageFilterBucketRegion>;
  endTime?: InputMaybe<AudioImageFilterEndTime>;
  feed?: InputMaybe<FeedFilterInput>;
  feedId?: InputMaybe<AudioImageFilterFeedId>;
  feedSegments?: InputMaybe<FeedSegmentFilterInput>;
  id?: InputMaybe<AudioImageFilterId>;
  imageSize?: InputMaybe<AudioImageFilterImageSize>;
  imageType?: InputMaybe<AudioImageFilterImageType>;
  not?: InputMaybe<Array<AudioImageFilterInput>>;
  objectPath?: InputMaybe<AudioImageFilterObjectPath>;
  or?: InputMaybe<Array<AudioImageFilterInput>>;
  /** Parameters used for generating the image (e.g. n_fft for spectrograms, etc) */
  parameters?: InputMaybe<AudioImageFilterParameters>;
  startTime?: InputMaybe<AudioImageFilterStartTime>;
  status?: InputMaybe<AudioImageFilterStatus>;
};

export type AudioImageFilterObjectPath = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type AudioImageFilterParameters = {
  eq?: InputMaybe<Scalars["Json"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Json"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Json"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Json"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Json"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Json"]["input"]>;
  notEq?: InputMaybe<Scalars["Json"]["input"]>;
};

export type AudioImageFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type AudioImageFilterStatus = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type AudioImageSortField =
  | "BUCKET"
  | "BUCKET_REGION"
  | "END_TIME"
  | "FEED_ID"
  | "ID"
  | "IMAGE_SIZE"
  | "IMAGE_TYPE"
  | "OBJECT_PATH"
  | "PARAMETERS"
  | "START_TIME"
  | "STATUS";

export type AudioImageSortInput = {
  field: AudioImageSortField;
  order?: InputMaybe<SortOrder>;
};

export type Bout = {
  __typename?: "Bout";
  category?: Maybe<AudioCategory>;
  duration?: Maybe<Scalars["Decimal"]["output"]>;
  endTime?: Maybe<Scalars["DateTime"]["output"]>;
  id: Scalars["ID"]["output"];
  ongoing?: Maybe<Scalars["Boolean"]["output"]>;
  startTime?: Maybe<Scalars["DateTime"]["output"]>;
};

/** Join table between Bout and FeedStream */
export type BoutFeedStream = {
  __typename?: "BoutFeedStream";
  id: Scalars["ID"]["output"];
};

export type BoutFeedStreamFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type BoutFeedStreamFilterInput = {
  and?: InputMaybe<Array<BoutFeedStreamFilterInput>>;
  id?: InputMaybe<BoutFeedStreamFilterId>;
  not?: InputMaybe<Array<BoutFeedStreamFilterInput>>;
  or?: InputMaybe<Array<BoutFeedStreamFilterInput>>;
};

export type BoutFeedStreamSortField = "ID";

export type BoutFeedStreamSortInput = {
  field: BoutFeedStreamSortField;
  order?: InputMaybe<SortOrder>;
};

export type BoutFilterCategory = {
  eq?: InputMaybe<AudioCategory>;
  greaterThan?: InputMaybe<AudioCategory>;
  greaterThanOrEqual?: InputMaybe<AudioCategory>;
  in?: InputMaybe<Array<InputMaybe<AudioCategory>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<AudioCategory>;
  lessThanOrEqual?: InputMaybe<AudioCategory>;
  notEq?: InputMaybe<AudioCategory>;
};

export type BoutFilterDuration = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Decimal"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type BoutFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type BoutFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type BoutFilterInput = {
  and?: InputMaybe<Array<BoutFilterInput>>;
  category?: InputMaybe<BoutFilterCategory>;
  duration?: InputMaybe<BoutFilterDuration>;
  endTime?: InputMaybe<BoutFilterEndTime>;
  id?: InputMaybe<BoutFilterId>;
  not?: InputMaybe<Array<BoutFilterInput>>;
  ongoing?: InputMaybe<BoutFilterOngoing>;
  or?: InputMaybe<Array<BoutFilterInput>>;
  startTime?: InputMaybe<BoutFilterStartTime>;
};

export type BoutFilterOngoing = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type BoutFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type BoutSortField =
  | "CATEGORY"
  | "DURATION"
  | "END_TIME"
  | "ID"
  | "ONGOING"
  | "START_TIME";

export type BoutSortInput = {
  field: BoutSortField;
  order?: InputMaybe<SortOrder>;
};

export type CancelCandidateNotificationsInput = {
  eventType?: InputMaybe<NotificationEventType>;
};

/** The result of the :cancel_candidate_notifications mutation */
export type CancelCandidateNotificationsResult = {
  __typename?: "CancelCandidateNotificationsResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Candidate>;
};

/** The result of the :cancel_notification mutation */
export type CancelNotificationResult = {
  __typename?: "CancelNotificationResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Notification>;
};

export type Candidate = {
  __typename?: "Candidate";
  category?: Maybe<DetectionCategory>;
  detectionCount?: Maybe<Scalars["Int"]["output"]>;
  detections: Array<Detection>;
  feed: Feed;
  feedId: Scalars["ID"]["output"];
  id: Scalars["ID"]["output"];
  maxTime: Scalars["DateTime"]["output"];
  minTime: Scalars["DateTime"]["output"];
  visible?: Maybe<Scalars["Boolean"]["output"]>;
};

export type CandidateDetectionsArgs = {
  filter?: InputMaybe<DetectionFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<DetectionSortInput>>>;
};

export type CandidateFilterCategory = {
  eq?: InputMaybe<DetectionCategory>;
  greaterThan?: InputMaybe<DetectionCategory>;
  greaterThanOrEqual?: InputMaybe<DetectionCategory>;
  in?: InputMaybe<Array<InputMaybe<DetectionCategory>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<DetectionCategory>;
  lessThanOrEqual?: InputMaybe<DetectionCategory>;
  notEq?: InputMaybe<DetectionCategory>;
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

export type CandidateFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type CandidateFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type CandidateFilterInput = {
  and?: InputMaybe<Array<CandidateFilterInput>>;
  category?: InputMaybe<CandidateFilterCategory>;
  detectionCount?: InputMaybe<CandidateFilterDetectionCount>;
  detections?: InputMaybe<DetectionFilterInput>;
  feed?: InputMaybe<FeedFilterInput>;
  feedId?: InputMaybe<CandidateFilterFeedId>;
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
  | "CATEGORY"
  | "DETECTION_COUNT"
  | "FEED_ID"
  | "ID"
  | "MAX_TIME"
  | "MIN_TIME"
  | "VISIBLE";

export type CandidateSortInput = {
  field: CandidateSortField;
  order?: InputMaybe<SortOrder>;
};

/** The result of the :create_bout mutation */
export type CreateBoutResult = {
  __typename?: "CreateBoutResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Bout>;
};

export type Detection = {
  __typename?: "Detection";
  candidate?: Maybe<Candidate>;
  candidateId?: Maybe<Scalars["ID"]["output"]>;
  category?: Maybe<DetectionCategory>;
  description?: Maybe<Scalars["String"]["output"]>;
  feed?: Maybe<Feed>;
  feedId?: Maybe<Scalars["ID"]["output"]>;
  id: Scalars["ID"]["output"];
  listenerCount?: Maybe<Scalars["Int"]["output"]>;
  playerOffset: Scalars["Decimal"]["output"];
  playlistTimestamp: Scalars["Int"]["output"];
  sourceIp?: Maybe<Scalars["String"]["output"]>;
  timestamp: Scalars["DateTime"]["output"];
  visible?: Maybe<Scalars["Boolean"]["output"]>;
};

export type DetectionCategory = "OTHER" | "VESSEL" | "WHALE";

export type DetectionFilterCandidateId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

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
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type DetectionFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type DetectionFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type DetectionFilterInput = {
  and?: InputMaybe<Array<DetectionFilterInput>>;
  candidate?: InputMaybe<CandidateFilterInput>;
  candidateId?: InputMaybe<DetectionFilterCandidateId>;
  category?: InputMaybe<DetectionFilterCategory>;
  description?: InputMaybe<DetectionFilterDescription>;
  feed?: InputMaybe<FeedFilterInput>;
  feedId?: InputMaybe<DetectionFilterFeedId>;
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
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
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
  | "CANDIDATE_ID"
  | "CATEGORY"
  | "DESCRIPTION"
  | "FEED_ID"
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
  audioImages: Array<AudioImage>;
  bouts: Array<Bout>;
  bucket: Scalars["String"]["output"];
  bucketRegion?: Maybe<Scalars["String"]["output"]>;
  cloudfrontUrl?: Maybe<Scalars["String"]["output"]>;
  dataplicityId?: Maybe<Scalars["String"]["output"]>;
  feedSegments: Array<FeedSegment>;
  feedStreams: Array<FeedStream>;
  id: Scalars["ID"]["output"];
  imageUrl?: Maybe<Scalars["String"]["output"]>;
  introHtml?: Maybe<Scalars["String"]["output"]>;
  latLng: LatLng;
  locationPoint: Scalars["Json"]["output"];
  mapUrl?: Maybe<Scalars["String"]["output"]>;
  name: Scalars["String"]["output"];
  nodeName: Scalars["String"]["output"];
  online?: Maybe<Scalars["Boolean"]["output"]>;
  orcahelloId?: Maybe<Scalars["String"]["output"]>;
  slug: Scalars["String"]["output"];
  thumbUrl?: Maybe<Scalars["String"]["output"]>;
  visible?: Maybe<Scalars["Boolean"]["output"]>;
};

export type FeedAudioImagesArgs = {
  filter?: InputMaybe<AudioImageFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageSortInput>>>;
};

export type FeedBoutsArgs = {
  filter?: InputMaybe<BoutFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<BoutSortInput>>>;
};

export type FeedFeedSegmentsArgs = {
  filter?: InputMaybe<FeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedSegmentSortInput>>>;
};

export type FeedFeedStreamsArgs = {
  filter?: InputMaybe<FeedStreamFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedStreamSortInput>>>;
};

export type FeedFilterBucket = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterBucketRegion = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterCloudfrontUrl = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterDataplicityId = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterImageUrl = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterInput = {
  and?: InputMaybe<Array<FeedFilterInput>>;
  audioImages?: InputMaybe<AudioImageFilterInput>;
  bouts?: InputMaybe<BoutFilterInput>;
  bucket?: InputMaybe<FeedFilterBucket>;
  bucketRegion?: InputMaybe<FeedFilterBucketRegion>;
  cloudfrontUrl?: InputMaybe<FeedFilterCloudfrontUrl>;
  dataplicityId?: InputMaybe<FeedFilterDataplicityId>;
  feedSegments?: InputMaybe<FeedSegmentFilterInput>;
  feedStreams?: InputMaybe<FeedStreamFilterInput>;
  id?: InputMaybe<FeedFilterId>;
  imageUrl?: InputMaybe<FeedFilterImageUrl>;
  introHtml?: InputMaybe<FeedFilterIntroHtml>;
  locationPoint?: InputMaybe<FeedFilterLocationPoint>;
  name?: InputMaybe<FeedFilterName>;
  nodeName?: InputMaybe<FeedFilterNodeName>;
  not?: InputMaybe<Array<FeedFilterInput>>;
  online?: InputMaybe<FeedFilterOnline>;
  or?: InputMaybe<Array<FeedFilterInput>>;
  orcahelloId?: InputMaybe<FeedFilterOrcahelloId>;
  slug?: InputMaybe<FeedFilterSlug>;
  visible?: InputMaybe<FeedFilterVisible>;
};

export type FeedFilterIntroHtml = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterLocationPoint = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterNodeName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterOnline = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterOrcahelloId = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterSlug = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
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

export type FeedSegment = {
  __typename?: "FeedSegment";
  audioImageFeedSegments: Array<AudioImageFeedSegment>;
  audioImages: Array<AudioImage>;
  bucket?: Maybe<Scalars["String"]["output"]>;
  bucketRegion?: Maybe<Scalars["String"]["output"]>;
  cloudfrontUrl?: Maybe<Scalars["String"]["output"]>;
  duration?: Maybe<Scalars["Decimal"]["output"]>;
  endTime?: Maybe<Scalars["DateTime"]["output"]>;
  feed?: Maybe<Feed>;
  feedId?: Maybe<Scalars["ID"]["output"]>;
  feedStream?: Maybe<FeedStream>;
  feedStreamId?: Maybe<Scalars["ID"]["output"]>;
  /** ts file name (e.g. live005.ts) */
  fileName: Scalars["String"]["output"];
  id: Scalars["ID"]["output"];
  /** S3 object path for playlist file (e.g. /rpi_orcasound_lab/hls/1541027406/live.m3u8) */
  playlistM3u8Path?: Maybe<Scalars["String"]["output"]>;
  /** S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/) */
  playlistPath?: Maybe<Scalars["String"]["output"]>;
  /** UTC Unix epoch for playlist (m3u8 dir) start (e.g. 1541027406) */
  playlistTimestamp?: Maybe<Scalars["String"]["output"]>;
  /** S3 object path for ts file (e.g. /rpi_orcasound_lab/hls/1541027406/live005.ts) */
  segmentPath?: Maybe<Scalars["String"]["output"]>;
  startTime?: Maybe<Scalars["DateTime"]["output"]>;
};

export type FeedSegmentAudioImageFeedSegmentsArgs = {
  filter?: InputMaybe<AudioImageFeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageFeedSegmentSortInput>>>;
};

export type FeedSegmentAudioImagesArgs = {
  filter?: InputMaybe<AudioImageFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageSortInput>>>;
};

export type FeedSegmentFilterBucket = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterBucketRegion = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterCloudfrontUrl = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterDuration = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Decimal"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type FeedSegmentFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type FeedSegmentFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedSegmentFilterFeedStreamId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedSegmentFilterFileName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedSegmentFilterInput = {
  and?: InputMaybe<Array<FeedSegmentFilterInput>>;
  audioImageFeedSegments?: InputMaybe<AudioImageFeedSegmentFilterInput>;
  audioImages?: InputMaybe<AudioImageFilterInput>;
  bucket?: InputMaybe<FeedSegmentFilterBucket>;
  bucketRegion?: InputMaybe<FeedSegmentFilterBucketRegion>;
  cloudfrontUrl?: InputMaybe<FeedSegmentFilterCloudfrontUrl>;
  duration?: InputMaybe<FeedSegmentFilterDuration>;
  endTime?: InputMaybe<FeedSegmentFilterEndTime>;
  feed?: InputMaybe<FeedFilterInput>;
  feedId?: InputMaybe<FeedSegmentFilterFeedId>;
  feedStream?: InputMaybe<FeedStreamFilterInput>;
  feedStreamId?: InputMaybe<FeedSegmentFilterFeedStreamId>;
  /** ts file name (e.g. live005.ts) */
  fileName?: InputMaybe<FeedSegmentFilterFileName>;
  id?: InputMaybe<FeedSegmentFilterId>;
  not?: InputMaybe<Array<FeedSegmentFilterInput>>;
  or?: InputMaybe<Array<FeedSegmentFilterInput>>;
  /** S3 object path for playlist file (e.g. /rpi_orcasound_lab/hls/1541027406/live.m3u8) */
  playlistM3u8Path?: InputMaybe<FeedSegmentFilterPlaylistM3u8Path>;
  /** S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/) */
  playlistPath?: InputMaybe<FeedSegmentFilterPlaylistPath>;
  /** UTC Unix epoch for playlist (m3u8 dir) start (e.g. 1541027406) */
  playlistTimestamp?: InputMaybe<FeedSegmentFilterPlaylistTimestamp>;
  /** S3 object path for ts file (e.g. /rpi_orcasound_lab/hls/1541027406/live005.ts) */
  segmentPath?: InputMaybe<FeedSegmentFilterSegmentPath>;
  startTime?: InputMaybe<FeedSegmentFilterStartTime>;
};

export type FeedSegmentFilterPlaylistM3u8Path = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterPlaylistPath = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterPlaylistTimestamp = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterSegmentPath = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type FeedSegmentSortField =
  | "BUCKET"
  | "BUCKET_REGION"
  | "CLOUDFRONT_URL"
  | "DURATION"
  | "END_TIME"
  | "FEED_ID"
  | "FEED_STREAM_ID"
  | "FILE_NAME"
  | "ID"
  | "PLAYLIST_M3U8_PATH"
  | "PLAYLIST_PATH"
  | "PLAYLIST_TIMESTAMP"
  | "SEGMENT_PATH"
  | "START_TIME";

export type FeedSegmentSortInput = {
  field: FeedSegmentSortField;
  order?: InputMaybe<SortOrder>;
};

export type FeedSortField =
  | "BUCKET"
  | "BUCKET_REGION"
  | "CLOUDFRONT_URL"
  | "DATAPLICITY_ID"
  | "ID"
  | "IMAGE_URL"
  | "INTRO_HTML"
  | "LOCATION_POINT"
  | "NAME"
  | "NODE_NAME"
  | "ONLINE"
  | "ORCAHELLO_ID"
  | "SLUG"
  | "VISIBLE";

export type FeedSortInput = {
  field: FeedSortField;
  order?: InputMaybe<SortOrder>;
};

export type FeedStream = {
  __typename?: "FeedStream";
  boutFeedStreams: Array<BoutFeedStream>;
  bouts: Array<Bout>;
  bucket?: Maybe<Scalars["String"]["output"]>;
  bucketRegion?: Maybe<Scalars["String"]["output"]>;
  cloudfrontUrl?: Maybe<Scalars["String"]["output"]>;
  duration?: Maybe<Scalars["Decimal"]["output"]>;
  endTime?: Maybe<Scalars["DateTime"]["output"]>;
  feed?: Maybe<Feed>;
  feedId?: Maybe<Scalars["String"]["output"]>;
  feedSegments: Array<FeedSegment>;
  id: Scalars["ID"]["output"];
  nextFeedStream?: Maybe<FeedStream>;
  nextFeedStreamId?: Maybe<Scalars["String"]["output"]>;
  /** S3 object path for playlist file (e.g. /rpi_orcasound_lab/hls/1541027406/live.m3u8) */
  playlistM3u8Path?: Maybe<Scalars["String"]["output"]>;
  /** S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/) */
  playlistPath?: Maybe<Scalars["String"]["output"]>;
  /** UTC Unix epoch for playlist start (e.g. 1541027406) */
  playlistTimestamp?: Maybe<Scalars["String"]["output"]>;
  prevFeedStream?: Maybe<FeedStream>;
  prevFeedStreamId?: Maybe<Scalars["String"]["output"]>;
  startTime?: Maybe<Scalars["DateTime"]["output"]>;
};

export type FeedStreamBoutFeedStreamsArgs = {
  filter?: InputMaybe<BoutFeedStreamFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<BoutFeedStreamSortInput>>>;
};

export type FeedStreamBoutsArgs = {
  filter?: InputMaybe<BoutFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<BoutSortInput>>>;
};

export type FeedStreamFeedSegmentsArgs = {
  filter?: InputMaybe<FeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedSegmentSortInput>>>;
};

export type FeedStreamFilterBucket = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterBucketRegion = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterCloudfrontUrl = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterDuration = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Decimal"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type FeedStreamFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type FeedStreamFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedStreamFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedStreamFilterInput = {
  and?: InputMaybe<Array<FeedStreamFilterInput>>;
  boutFeedStreams?: InputMaybe<BoutFeedStreamFilterInput>;
  bouts?: InputMaybe<BoutFilterInput>;
  bucket?: InputMaybe<FeedStreamFilterBucket>;
  bucketRegion?: InputMaybe<FeedStreamFilterBucketRegion>;
  cloudfrontUrl?: InputMaybe<FeedStreamFilterCloudfrontUrl>;
  duration?: InputMaybe<FeedStreamFilterDuration>;
  endTime?: InputMaybe<FeedStreamFilterEndTime>;
  feed?: InputMaybe<FeedFilterInput>;
  feedId?: InputMaybe<FeedStreamFilterFeedId>;
  feedSegments?: InputMaybe<FeedSegmentFilterInput>;
  id?: InputMaybe<FeedStreamFilterId>;
  nextFeedStream?: InputMaybe<FeedStreamFilterInput>;
  nextFeedStreamId?: InputMaybe<FeedStreamFilterNextFeedStreamId>;
  not?: InputMaybe<Array<FeedStreamFilterInput>>;
  or?: InputMaybe<Array<FeedStreamFilterInput>>;
  /** S3 object path for playlist file (e.g. /rpi_orcasound_lab/hls/1541027406/live.m3u8) */
  playlistM3u8Path?: InputMaybe<FeedStreamFilterPlaylistM3u8Path>;
  /** S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/) */
  playlistPath?: InputMaybe<FeedStreamFilterPlaylistPath>;
  /** UTC Unix epoch for playlist start (e.g. 1541027406) */
  playlistTimestamp?: InputMaybe<FeedStreamFilterPlaylistTimestamp>;
  prevFeedStream?: InputMaybe<FeedStreamFilterInput>;
  prevFeedStreamId?: InputMaybe<FeedStreamFilterPrevFeedStreamId>;
  startTime?: InputMaybe<FeedStreamFilterStartTime>;
};

export type FeedStreamFilterNextFeedStreamId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedStreamFilterPlaylistM3u8Path = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterPlaylistPath = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterPlaylistTimestamp = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterPrevFeedStreamId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedStreamFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type FeedStreamSortField =
  | "BUCKET"
  | "BUCKET_REGION"
  | "CLOUDFRONT_URL"
  | "DURATION"
  | "END_TIME"
  | "FEED_ID"
  | "ID"
  | "NEXT_FEED_STREAM_ID"
  | "PLAYLIST_M3U8_PATH"
  | "PLAYLIST_PATH"
  | "PLAYLIST_TIMESTAMP"
  | "PREV_FEED_STREAM_ID"
  | "START_TIME";

export type FeedStreamSortInput = {
  field: FeedStreamSortField;
  order?: InputMaybe<SortOrder>;
};

export type GenerateFeedSpectrogramInput = {
  endTime: Scalars["DateTime"]["input"];
  startTime: Scalars["DateTime"]["input"];
};

/** The result of the :generate_feed_spectrogram mutation */
export type GenerateFeedSpectrogramResult = {
  __typename?: "GenerateFeedSpectrogramResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Feed>;
};

export type ImageType = "SPECTROGRAM";

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
  fields?: Maybe<Array<Scalars["String"]["output"]>>;
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
  finished?: Maybe<Scalars["Boolean"]["output"]>;
  id: Scalars["ID"]["output"];
  insertedAt: Scalars["DateTime"]["output"];
  notifiedCount?: Maybe<Scalars["Int"]["output"]>;
  notifiedCountUpdatedAt?: Maybe<Scalars["DateTime"]["output"]>;
  progress?: Maybe<Scalars["Float"]["output"]>;
  targetCount?: Maybe<Scalars["Int"]["output"]>;
};

export type NotificationEventType = "CONFIRMED_CANDIDATE" | "NEW_DETECTION";

export type NotificationFilterActive = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type NotificationFilterEventType = {
  eq?: InputMaybe<NotificationEventType>;
  greaterThan?: InputMaybe<NotificationEventType>;
  greaterThanOrEqual?: InputMaybe<NotificationEventType>;
  in?: InputMaybe<Array<InputMaybe<NotificationEventType>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<NotificationEventType>;
  lessThanOrEqual?: InputMaybe<NotificationEventType>;
  notEq?: InputMaybe<NotificationEventType>;
};

export type NotificationFilterFinished = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<Scalars["Boolean"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type NotificationFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type NotificationFilterInput = {
  active?: InputMaybe<NotificationFilterActive>;
  and?: InputMaybe<Array<NotificationFilterInput>>;
  eventType?: InputMaybe<NotificationFilterEventType>;
  finished?: InputMaybe<NotificationFilterFinished>;
  id?: InputMaybe<NotificationFilterId>;
  insertedAt?: InputMaybe<NotificationFilterInsertedAt>;
  not?: InputMaybe<Array<NotificationFilterInput>>;
  notifiedCount?: InputMaybe<NotificationFilterNotifiedCount>;
  notifiedCountUpdatedAt?: InputMaybe<NotificationFilterNotifiedCountUpdatedAt>;
  or?: InputMaybe<Array<NotificationFilterInput>>;
  progress?: InputMaybe<NotificationFilterProgress>;
  targetCount?: InputMaybe<NotificationFilterTargetCount>;
};

export type NotificationFilterInsertedAt = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type NotificationFilterNotifiedCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type NotificationFilterNotifiedCountUpdatedAt = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type NotificationFilterProgress = {
  eq?: InputMaybe<Scalars["Float"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Float"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Float"]["input"]>;
  in?: InputMaybe<Array<Scalars["Float"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Float"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Float"]["input"]>;
  notEq?: InputMaybe<Scalars["Float"]["input"]>;
};

export type NotificationFilterTargetCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type NotificationSortField =
  | "ACTIVE"
  | "EVENT_TYPE"
  | "FINISHED"
  | "ID"
  | "INSERTED_AT"
  | "NOTIFIED_COUNT"
  | "NOTIFIED_COUNT_UPDATED_AT"
  | "PROGRESS"
  | "TARGET_COUNT";

export type NotificationSortInput = {
  field: NotificationSortField;
  order?: InputMaybe<SortOrder>;
};

export type NotifyConfirmedCandidateInput = {
  candidateId: Scalars["String"]["input"];
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
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Notification>;
};

/** A page of :bout */
export type PageOfBout = {
  __typename?: "PageOfBout";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<Bout>>;
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

/** A page of :feed_segment */
export type PageOfFeedSegment = {
  __typename?: "PageOfFeedSegment";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<FeedSegment>>;
};

/** A page of :feed_stream */
export type PageOfFeedStream = {
  __typename?: "PageOfFeedStream";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<FeedStream>>;
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
  username?: InputMaybe<Scalars["String"]["input"]>;
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
  errors: Array<MutationError>;
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
  cancelCandidateNotifications: CancelCandidateNotificationsResult;
  cancelNotification: CancelNotificationResult;
  createBout: CreateBoutResult;
  generateFeedSpectrogram: GenerateFeedSpectrogramResult;
  /** Create a notification for confirmed candidate (i.e. detection group) */
  notifyConfirmedCandidate: NotifyConfirmedCandidateResult;
  /** Register a new user with a username and password. */
  registerWithPassword: RegisterWithPasswordResult;
  requestPasswordReset?: Maybe<Scalars["Boolean"]["output"]>;
  resetPassword?: Maybe<PasswordResetResult>;
  setDetectionVisible: SetDetectionVisibleResult;
  signInWithPassword?: Maybe<SignInWithPasswordResult>;
  signOut?: Maybe<Scalars["Boolean"]["output"]>;
  submitDetection: SubmitDetectionResult;
};

export type RootMutationTypeCancelCandidateNotificationsArgs = {
  id: Scalars["ID"]["input"];
  input?: InputMaybe<CancelCandidateNotificationsInput>;
};

export type RootMutationTypeCancelNotificationArgs = {
  id: Scalars["ID"]["input"];
};

export type RootMutationTypeGenerateFeedSpectrogramArgs = {
  id: Scalars["ID"]["input"];
  input: GenerateFeedSpectrogramInput;
};

export type RootMutationTypeNotifyConfirmedCandidateArgs = {
  input: NotifyConfirmedCandidateInput;
};

export type RootMutationTypeRegisterWithPasswordArgs = {
  input: RegisterWithPasswordInput;
};

export type RootMutationTypeRequestPasswordResetArgs = {
  input: RequestPasswordResetInput;
};

export type RootMutationTypeResetPasswordArgs = {
  input: PasswordResetInput;
};

export type RootMutationTypeSetDetectionVisibleArgs = {
  id: Scalars["ID"]["input"];
  input?: InputMaybe<SetDetectionVisibleInput>;
};

export type RootMutationTypeSignInWithPasswordArgs = {
  input: SignInWithPasswordInput;
};

export type RootMutationTypeSubmitDetectionArgs = {
  input: SubmitDetectionInput;
};

export type RootQueryType = {
  __typename?: "RootQueryType";
  bouts?: Maybe<PageOfBout>;
  candidate?: Maybe<Candidate>;
  candidates?: Maybe<PageOfCandidate>;
  currentUser?: Maybe<User>;
  detection?: Maybe<Detection>;
  detections?: Maybe<PageOfDetection>;
  feed: Feed;
  feedSegments?: Maybe<PageOfFeedSegment>;
  feedStreams?: Maybe<PageOfFeedStream>;
  feeds: Array<Feed>;
  notificationsForCandidate: Array<Notification>;
};

export type RootQueryTypeBoutsArgs = {
  feedId?: InputMaybe<Scalars["String"]["input"]>;
  filter?: InputMaybe<BoutFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<BoutSortInput>>>;
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

export type RootQueryTypeFeedSegmentsArgs = {
  feedId?: InputMaybe<Scalars["String"]["input"]>;
  feedStreamId?: InputMaybe<Scalars["String"]["input"]>;
  filter?: InputMaybe<FeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedSegmentSortInput>>>;
};

export type RootQueryTypeFeedStreamsArgs = {
  feedId?: InputMaybe<Scalars["String"]["input"]>;
  filter?: InputMaybe<FeedStreamFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedStreamSortInput>>>;
};

export type RootQueryTypeFeedsArgs = {
  filter?: InputMaybe<FeedFilterInput>;
  sort?: InputMaybe<Array<InputMaybe<FeedSortInput>>>;
};

export type RootQueryTypeNotificationsForCandidateArgs = {
  active?: InputMaybe<Scalars["Boolean"]["input"]>;
  candidateId: Scalars["String"]["input"];
  eventType?: InputMaybe<NotificationEventType>;
  filter?: InputMaybe<NotificationFilterInput>;
  sort?: InputMaybe<Array<InputMaybe<NotificationSortInput>>>;
};

export type SetDetectionVisibleInput = {
  visible?: InputMaybe<Scalars["Boolean"]["input"]>;
};

/** The result of the :set_detection_visible mutation */
export type SetDetectionVisibleResult = {
  __typename?: "SetDetectionVisibleResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
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
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Detection>;
};

export type User = {
  __typename?: "User";
  admin: Scalars["Boolean"]["output"];
  email: Scalars["String"]["output"];
  firstName?: Maybe<Scalars["String"]["output"]>;
  id: Scalars["ID"]["output"];
  lastName?: Maybe<Scalars["String"]["output"]>;
  moderator: Scalars["Boolean"]["output"];
  username?: Maybe<Scalars["String"]["output"]>;
};

export type UserFilterAdmin = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<Scalars["Boolean"]["input"]>>;
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
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
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
  username?: InputMaybe<UserFilterUsername>;
};

export type UserFilterLastName = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserFilterModerator = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<Scalars["Boolean"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type UserFilterUsername = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type CancelCandidateNotificationsMutationVariables = Exact<{
  candidateId: Scalars["ID"]["input"];
}>;

export type CancelCandidateNotificationsMutation = {
  __typename?: "RootMutationType";
  cancelCandidateNotifications: {
    __typename?: "CancelCandidateNotificationsResult";
    result?: { __typename?: "Candidate"; id: string } | null;
    errors: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    }>;
  };
};

export type CancelNotificationMutationVariables = Exact<{
  id: Scalars["ID"]["input"];
}>;

export type CancelNotificationMutation = {
  __typename?: "RootMutationType";
  cancelNotification: {
    __typename?: "CancelNotificationResult";
    result?: {
      __typename?: "Notification";
      id: string;
      active?: boolean | null;
      insertedAt: Date;
      targetCount?: number | null;
      notifiedCount?: number | null;
      notifiedCountUpdatedAt?: Date | null;
      progress?: number | null;
      finished?: boolean | null;
    } | null;
    errors: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    }>;
  };
};

export type NotifyConfirmedCandidateMutationVariables = Exact<{
  candidateId: Scalars["String"]["input"];
  message: Scalars["String"]["input"];
}>;

export type NotifyConfirmedCandidateMutation = {
  __typename?: "RootMutationType";
  notifyConfirmedCandidate: {
    __typename?: "NotifyConfirmedCandidateResult";
    result?: {
      __typename?: "Notification";
      id: string;
      eventType?: NotificationEventType | null;
      active?: boolean | null;
      targetCount?: number | null;
      notifiedCount?: number | null;
      progress?: number | null;
      finished?: boolean | null;
      notifiedCountUpdatedAt?: Date | null;
    } | null;
    errors: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    }>;
  };
};

export type RegisterWithPasswordMutationVariables = Exact<{
  firstName?: InputMaybe<Scalars["String"]["input"]>;
  lastName?: InputMaybe<Scalars["String"]["input"]>;
  email: Scalars["String"]["input"];
  username: Scalars["String"]["input"];
  password: Scalars["String"]["input"];
  passwordConfirmation: Scalars["String"]["input"];
}>;

export type RegisterWithPasswordMutation = {
  __typename?: "RootMutationType";
  registerWithPassword: {
    __typename?: "RegisterWithPasswordResult";
    result?: {
      __typename?: "User";
      id: string;
      email: string;
      username?: string | null;
      admin: boolean;
      firstName?: string | null;
      lastName?: string | null;
    } | null;
    errors: Array<{
      __typename?: "MutationError";
      message?: string | null;
      code?: string | null;
      fields?: Array<string> | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    }>;
  };
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
      fields?: Array<string> | null;
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
      admin: boolean;
    } | null;
  } | null;
};

export type SetDetectionVisibleMutationVariables = Exact<{
  id: Scalars["ID"]["input"];
  visible: Scalars["Boolean"]["input"];
}>;

export type SetDetectionVisibleMutation = {
  __typename?: "RootMutationType";
  setDetectionVisible: {
    __typename?: "SetDetectionVisibleResult";
    result?: {
      __typename?: "Detection";
      id: string;
      visible?: boolean | null;
    } | null;
    errors: Array<{
      __typename?: "MutationError";
      code?: string | null;
      fields?: Array<string> | null;
      message?: string | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    }>;
  };
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
      admin: boolean;
      firstName?: string | null;
      lastName?: string | null;
    } | null;
    errors?: Array<{
      __typename?: "MutationError";
      message?: string | null;
      code?: string | null;
      fields?: Array<string> | null;
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
  submitDetection: {
    __typename?: "SubmitDetectionResult";
    result?: { __typename?: "Detection"; id: string } | null;
    errors: Array<{
      __typename?: "MutationError";
      message?: string | null;
      code?: string | null;
      fields?: Array<string> | null;
      shortMessage?: string | null;
      vars?: { [key: string]: any } | null;
    }>;
  };
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
    category?: DetectionCategory | null;
    feed: {
      __typename?: "Feed";
      id: string;
      slug: string;
      name: string;
      nodeName: string;
      bucket: string;
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
      sourceIp?: string | null;
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
    admin: boolean;
    moderator: boolean;
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
    bucket: string;
    latLng: { __typename?: "LatLng"; lat: number; lng: number };
  };
};

export type NotificationsForCandidateQueryVariables = Exact<{
  candidateId: Scalars["String"]["input"];
  eventType?: InputMaybe<NotificationEventType>;
}>;

export type NotificationsForCandidateQuery = {
  __typename?: "RootQueryType";
  notificationsForCandidate: Array<{
    __typename?: "Notification";
    id: string;
    eventType?: NotificationEventType | null;
    active?: boolean | null;
    insertedAt: Date;
    targetCount?: number | null;
    notifiedCount?: number | null;
    notifiedCountUpdatedAt?: Date | null;
    progress?: number | null;
    finished?: boolean | null;
  }>;
};

export type Feeds2QueryVariables = Exact<{
  sort?: InputMaybe<
    Array<InputMaybe<FeedSortInput>> | InputMaybe<FeedSortInput>
  >;
}>;

export type Feeds2Query = {
  __typename?: "RootQueryType";
  feeds: Array<{
    __typename?: "Feed";
    id: string;
    name: string;
    slug: string;
    nodeName: string;
    imageUrl?: string | null;
    mapUrl?: string | null;
    thumbUrl?: string | null;
    bucket: string;
    online?: boolean | null;
    latLng: { __typename?: "LatLng"; lat: number; lng: number };
  }>;
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
      category?: DetectionCategory | null;
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
        sourceIp?: string | null;
      }>;
    }> | null;
  } | null;
};

export type DetectionsQueryVariables = Exact<{
  filter?: InputMaybe<DetectionFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<
    Array<InputMaybe<DetectionSortInput>> | InputMaybe<DetectionSortInput>
  >;
}>;

export type DetectionsQuery = {
  __typename?: "RootQueryType";
  detections?: {
    __typename?: "PageOfDetection";
    count?: number | null;
    hasNextPage: boolean;
    results?: Array<{
      __typename?: "Detection";
      id: string;
      feedId?: string | null;
      listenerCount?: number | null;
      category?: DetectionCategory | null;
      description?: string | null;
      playerOffset: number;
      playlistTimestamp: number;
      timestamp: Date;
      candidate?: { __typename?: "Candidate"; id: string } | null;
    }> | null;
  } | null;
};

export type FeedsQueryVariables = Exact<{
  sort?: InputMaybe<
    Array<InputMaybe<FeedSortInput>> | InputMaybe<FeedSortInput>
  >;
}>;

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
    bucket: string;
    online?: boolean | null;
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
) => {
  return useMutation<
    CancelCandidateNotificationsMutation,
    TError,
    CancelCandidateNotificationsMutationVariables,
    TContext
  >({
    mutationKey: ["cancelCandidateNotifications"],
    mutationFn: (variables?: CancelCandidateNotificationsMutationVariables) =>
      fetcher<
        CancelCandidateNotificationsMutation,
        CancelCandidateNotificationsMutationVariables
      >(CancelCandidateNotificationsDocument, variables)(),
    ...options,
  });
};

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

export const CancelNotificationDocument = `
    mutation cancelNotification($id: ID!) {
  cancelNotification(id: $id) {
    result {
      id
      active
      insertedAt
      targetCount
      notifiedCount
      notifiedCountUpdatedAt
      progress
      finished
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

export const useCancelNotificationMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    CancelNotificationMutation,
    TError,
    CancelNotificationMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    CancelNotificationMutation,
    TError,
    CancelNotificationMutationVariables,
    TContext
  >({
    mutationKey: ["cancelNotification"],
    mutationFn: (variables?: CancelNotificationMutationVariables) =>
      fetcher<CancelNotificationMutation, CancelNotificationMutationVariables>(
        CancelNotificationDocument,
        variables,
      )(),
    ...options,
  });
};

useCancelNotificationMutation.getKey = () => ["cancelNotification"];

useCancelNotificationMutation.fetcher = (
  variables: CancelNotificationMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<CancelNotificationMutation, CancelNotificationMutationVariables>(
    CancelNotificationDocument,
    variables,
    options,
  );

export const NotifyConfirmedCandidateDocument = `
    mutation notifyConfirmedCandidate($candidateId: String!, $message: String!) {
  notifyConfirmedCandidate(input: {candidateId: $candidateId, message: $message}) {
    result {
      id
      eventType
      active
      targetCount
      notifiedCount
      progress
      finished
      notifiedCountUpdatedAt
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
) => {
  return useMutation<
    NotifyConfirmedCandidateMutation,
    TError,
    NotifyConfirmedCandidateMutationVariables,
    TContext
  >({
    mutationKey: ["notifyConfirmedCandidate"],
    mutationFn: (variables?: NotifyConfirmedCandidateMutationVariables) =>
      fetcher<
        NotifyConfirmedCandidateMutation,
        NotifyConfirmedCandidateMutationVariables
      >(NotifyConfirmedCandidateDocument, variables)(),
    ...options,
  });
};

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
    mutation registerWithPassword($firstName: String, $lastName: String, $email: String!, $username: String!, $password: String!, $passwordConfirmation: String!) {
  registerWithPassword(
    input: {email: $email, username: $username, password: $password, passwordConfirmation: $passwordConfirmation, firstName: $firstName, lastName: $lastName}
  ) {
    result {
      id
      email
      username
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
) => {
  return useMutation<
    RegisterWithPasswordMutation,
    TError,
    RegisterWithPasswordMutationVariables,
    TContext
  >({
    mutationKey: ["registerWithPassword"],
    mutationFn: (variables?: RegisterWithPasswordMutationVariables) =>
      fetcher<
        RegisterWithPasswordMutation,
        RegisterWithPasswordMutationVariables
      >(RegisterWithPasswordDocument, variables)(),
    ...options,
  });
};

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
) => {
  return useMutation<
    RequestPasswordResetMutation,
    TError,
    RequestPasswordResetMutationVariables,
    TContext
  >({
    mutationKey: ["requestPasswordReset"],
    mutationFn: (variables?: RequestPasswordResetMutationVariables) =>
      fetcher<
        RequestPasswordResetMutation,
        RequestPasswordResetMutationVariables
      >(RequestPasswordResetDocument, variables)(),
    ...options,
  });
};

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
) => {
  return useMutation<
    ResetPasswordMutation,
    TError,
    ResetPasswordMutationVariables,
    TContext
  >({
    mutationKey: ["resetPassword"],
    mutationFn: (variables?: ResetPasswordMutationVariables) =>
      fetcher<ResetPasswordMutation, ResetPasswordMutationVariables>(
        ResetPasswordDocument,
        variables,
      )(),
    ...options,
  });
};

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
) => {
  return useMutation<
    SetDetectionVisibleMutation,
    TError,
    SetDetectionVisibleMutationVariables,
    TContext
  >({
    mutationKey: ["setDetectionVisible"],
    mutationFn: (variables?: SetDetectionVisibleMutationVariables) =>
      fetcher<
        SetDetectionVisibleMutation,
        SetDetectionVisibleMutationVariables
      >(SetDetectionVisibleDocument, variables)(),
    ...options,
  });
};

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
) => {
  return useMutation<
    SignInWithPasswordMutation,
    TError,
    SignInWithPasswordMutationVariables,
    TContext
  >({
    mutationKey: ["signInWithPassword"],
    mutationFn: (variables?: SignInWithPasswordMutationVariables) =>
      fetcher<SignInWithPasswordMutation, SignInWithPasswordMutationVariables>(
        SignInWithPasswordDocument,
        variables,
      )(),
    ...options,
  });
};

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
) => {
  return useMutation<
    SignOutMutation,
    TError,
    SignOutMutationVariables,
    TContext
  >({
    mutationKey: ["signOut"],
    mutationFn: (variables?: SignOutMutationVariables) =>
      fetcher<SignOutMutation, SignOutMutationVariables>(
        SignOutDocument,
        variables,
      )(),
    ...options,
  });
};

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
) => {
  return useMutation<
    SubmitDetectionMutation,
    TError,
    SubmitDetectionMutationVariables,
    TContext
  >({
    mutationKey: ["submitDetection"],
    mutationFn: (variables?: SubmitDetectionMutationVariables) =>
      fetcher<SubmitDetectionMutation, SubmitDetectionMutationVariables>(
        SubmitDetectionDocument,
        variables,
      )(),
    ...options,
  });
};

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
    category
    feed {
      id
      slug
      name
      nodeName
      bucket
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
      sourceIp
    }
  }
}
    `;

export const useCandidateQuery = <TData = CandidateQuery, TError = unknown>(
  variables: CandidateQueryVariables,
  options?: Omit<UseQueryOptions<CandidateQuery, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<CandidateQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<CandidateQuery, TError, TData>({
    queryKey: ["candidate", variables],
    queryFn: fetcher<CandidateQuery, CandidateQueryVariables>(
      CandidateDocument,
      variables,
    ),
    ...options,
  });
};

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
  options?: Omit<
    UseQueryOptions<GetCurrentUserQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<GetCurrentUserQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<GetCurrentUserQuery, TError, TData>({
    queryKey:
      variables === undefined
        ? ["getCurrentUser"]
        : ["getCurrentUser", variables],
    queryFn: fetcher<GetCurrentUserQuery, GetCurrentUserQueryVariables>(
      GetCurrentUserDocument,
      variables,
    ),
    ...options,
  });
};

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
    bucket
  }
}
    `;

export const useFeedQuery = <TData = FeedQuery, TError = unknown>(
  variables: FeedQueryVariables,
  options?: Omit<UseQueryOptions<FeedQuery, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<FeedQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<FeedQuery, TError, TData>({
    queryKey: ["feed", variables],
    queryFn: fetcher<FeedQuery, FeedQueryVariables>(FeedDocument, variables),
    ...options,
  });
};

useFeedQuery.document = FeedDocument;

useFeedQuery.getKey = (variables: FeedQueryVariables) => ["feed", variables];

useFeedQuery.fetcher = (
  variables: FeedQueryVariables,
  options?: RequestInit["headers"],
) => fetcher<FeedQuery, FeedQueryVariables>(FeedDocument, variables, options);

export const NotificationsForCandidateDocument = `
    query notificationsForCandidate($candidateId: String!, $eventType: NotificationEventType) {
  notificationsForCandidate(candidateId: $candidateId, eventType: $eventType) {
    id
    eventType
    active
    insertedAt
    targetCount
    notifiedCount
    notifiedCountUpdatedAt
    progress
    finished
  }
}
    `;

export const useNotificationsForCandidateQuery = <
  TData = NotificationsForCandidateQuery,
  TError = unknown,
>(
  variables: NotificationsForCandidateQueryVariables,
  options?: Omit<
    UseQueryOptions<NotificationsForCandidateQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<
      NotificationsForCandidateQuery,
      TError,
      TData
    >["queryKey"];
  },
) => {
  return useQuery<NotificationsForCandidateQuery, TError, TData>({
    queryKey: ["notificationsForCandidate", variables],
    queryFn: fetcher<
      NotificationsForCandidateQuery,
      NotificationsForCandidateQueryVariables
    >(NotificationsForCandidateDocument, variables),
    ...options,
  });
};

useNotificationsForCandidateQuery.document = NotificationsForCandidateDocument;

useNotificationsForCandidateQuery.getKey = (
  variables: NotificationsForCandidateQueryVariables,
) => ["notificationsForCandidate", variables];

useNotificationsForCandidateQuery.fetcher = (
  variables: NotificationsForCandidateQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<
    NotificationsForCandidateQuery,
    NotificationsForCandidateQueryVariables
  >(NotificationsForCandidateDocument, variables, options);

export const Feeds2Document = `
    query feeds2($sort: [FeedSortInput]) {
  feeds(sort: $sort) {
    id
    name
    slug
    nodeName
    latLng {
      lat
      lng
    }
    imageUrl
    mapUrl
    thumbUrl
    bucket
    online
  }
}
    `;

export const useFeeds2Query = <TData = Feeds2Query, TError = unknown>(
  variables?: Feeds2QueryVariables,
  options?: Omit<UseQueryOptions<Feeds2Query, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<Feeds2Query, TError, TData>["queryKey"];
  },
) => {
  return useQuery<Feeds2Query, TError, TData>({
    queryKey: variables === undefined ? ["feeds2"] : ["feeds2", variables],
    queryFn: fetcher<Feeds2Query, Feeds2QueryVariables>(
      Feeds2Document,
      variables,
    ),
    ...options,
  });
};

useFeeds2Query.document = Feeds2Document;

useFeeds2Query.getKey = (variables?: Feeds2QueryVariables) =>
  variables === undefined ? ["feeds2"] : ["feeds2", variables];

useFeeds2Query.fetcher = (
  variables?: Feeds2QueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<Feeds2Query, Feeds2QueryVariables>(
    Feeds2Document,
    variables,
    options,
  );

export const CandidatesDocument = `
    query candidates($filter: CandidateFilterInput, $limit: Int, $offset: Int, $sort: [CandidateSortInput]) {
  candidates(filter: $filter, limit: $limit, offset: $offset, sort: $sort) {
    count
    hasNextPage
    results {
      id
      minTime
      maxTime
      category
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
        sourceIp
      }
    }
  }
}
    `;

export const useCandidatesQuery = <TData = CandidatesQuery, TError = unknown>(
  variables?: CandidatesQueryVariables,
  options?: Omit<
    UseQueryOptions<CandidatesQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<CandidatesQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<CandidatesQuery, TError, TData>({
    queryKey:
      variables === undefined ? ["candidates"] : ["candidates", variables],
    queryFn: fetcher<CandidatesQuery, CandidatesQueryVariables>(
      CandidatesDocument,
      variables,
    ),
    ...options,
  });
};

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

export const DetectionsDocument = `
    query detections($filter: DetectionFilterInput, $limit: Int, $offset: Int, $sort: [DetectionSortInput]) {
  detections(filter: $filter, limit: $limit, offset: $offset, sort: $sort) {
    count
    hasNextPage
    results {
      id
      feedId
      listenerCount
      category
      description
      playerOffset
      playlistTimestamp
      timestamp
      candidate {
        id
      }
    }
  }
}
    `;

export const useDetectionsQuery = <TData = DetectionsQuery, TError = unknown>(
  variables?: DetectionsQueryVariables,
  options?: Omit<
    UseQueryOptions<DetectionsQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<DetectionsQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<DetectionsQuery, TError, TData>({
    queryKey:
      variables === undefined ? ["detections"] : ["detections", variables],
    queryFn: fetcher<DetectionsQuery, DetectionsQueryVariables>(
      DetectionsDocument,
      variables,
    ),
    ...options,
  });
};

useDetectionsQuery.document = DetectionsDocument;

useDetectionsQuery.getKey = (variables?: DetectionsQueryVariables) =>
  variables === undefined ? ["detections"] : ["detections", variables];

useDetectionsQuery.fetcher = (
  variables?: DetectionsQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<DetectionsQuery, DetectionsQueryVariables>(
    DetectionsDocument,
    variables,
    options,
  );

export const FeedsDocument = `
    query feeds($sort: [FeedSortInput]) {
  feeds(sort: $sort) {
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
    bucket
    online
  }
}
    `;

export const useFeedsQuery = <TData = FeedsQuery, TError = unknown>(
  variables?: FeedsQueryVariables,
  options?: Omit<UseQueryOptions<FeedsQuery, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<FeedsQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<FeedsQuery, TError, TData>({
    queryKey: variables === undefined ? ["feeds"] : ["feeds", variables],
    queryFn: fetcher<FeedsQuery, FeedsQueryVariables>(FeedsDocument, variables),
    ...options,
  });
};

useFeedsQuery.document = FeedsDocument;

useFeedsQuery.getKey = (variables?: FeedsQueryVariables) =>
  variables === undefined ? ["feeds"] : ["feeds", variables];

useFeedsQuery.fetcher = (
  variables?: FeedsQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<FeedsQuery, FeedsQueryVariables>(FeedsDocument, variables, options);
