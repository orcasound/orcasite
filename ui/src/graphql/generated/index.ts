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

export const AudioCategory = {
  Anthrophony: "ANTHROPHONY",
  Biophony: "BIOPHONY",
  Geophony: "GEOPHONY",
} as const;

export type AudioCategory = (typeof AudioCategory)[keyof typeof AudioCategory];
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

export const AudioImageFeedSegmentSortField = {
  AudioImageId: "AUDIO_IMAGE_ID",
  FeedSegmentId: "FEED_SEGMENT_ID",
  Id: "ID",
} as const;

export type AudioImageFeedSegmentSortField =
  (typeof AudioImageFeedSegmentSortField)[keyof typeof AudioImageFeedSegmentSortField];
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

export const AudioImageSortField = {
  Bucket: "BUCKET",
  BucketRegion: "BUCKET_REGION",
  EndTime: "END_TIME",
  FeedId: "FEED_ID",
  Id: "ID",
  ImageSize: "IMAGE_SIZE",
  ImageType: "IMAGE_TYPE",
  ObjectPath: "OBJECT_PATH",
  Parameters: "PARAMETERS",
  StartTime: "START_TIME",
  Status: "STATUS",
} as const;

export type AudioImageSortField =
  (typeof AudioImageSortField)[keyof typeof AudioImageSortField];
export type AudioImageSortInput = {
  field: AudioImageSortField;
  order?: InputMaybe<SortOrder>;
};

export type Bout = {
  __typename?: "Bout";
  category: AudioCategory;
  duration?: Maybe<Scalars["Decimal"]["output"]>;
  endTime?: Maybe<Scalars["DateTime"]["output"]>;
  feed?: Maybe<Feed>;
  feedId?: Maybe<Scalars["ID"]["output"]>;
  feedStreams: Array<FeedStream>;
  id: Scalars["ID"]["output"];
  itemTags: Array<ItemTag>;
  name?: Maybe<Scalars["String"]["output"]>;
  startTime: Scalars["DateTime"]["output"];
  tags: Array<Tag>;
};

export type BoutFeedStreamsArgs = {
  filter?: InputMaybe<FeedStreamFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedStreamSortInput>>>;
};

export type BoutItemTagsArgs = {
  filter?: InputMaybe<ItemTagFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<ItemTagSortInput>>>;
};

export type BoutTagsArgs = {
  filter?: InputMaybe<TagFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<TagSortInput>>>;
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

export const BoutFeedStreamSortField = {
  Id: "ID",
} as const;

export type BoutFeedStreamSortField =
  (typeof BoutFeedStreamSortField)[keyof typeof BoutFeedStreamSortField];
export type BoutFeedStreamSortInput = {
  field: BoutFeedStreamSortField;
  order?: InputMaybe<SortOrder>;
};

export type BoutFilterCategory = {
  eq?: InputMaybe<AudioCategory>;
  greaterThan?: InputMaybe<AudioCategory>;
  greaterThanOrEqual?: InputMaybe<AudioCategory>;
  in?: InputMaybe<Array<AudioCategory>>;
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

export type BoutFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type BoutFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type BoutFilterInput = {
  and?: InputMaybe<Array<BoutFilterInput>>;
  category?: InputMaybe<BoutFilterCategory>;
  duration?: InputMaybe<BoutFilterDuration>;
  endTime?: InputMaybe<BoutFilterEndTime>;
  feed?: InputMaybe<FeedFilterInput>;
  feedId?: InputMaybe<BoutFilterFeedId>;
  feedStreams?: InputMaybe<FeedStreamFilterInput>;
  id?: InputMaybe<BoutFilterId>;
  itemTags?: InputMaybe<ItemTagFilterInput>;
  name?: InputMaybe<BoutFilterName>;
  not?: InputMaybe<Array<BoutFilterInput>>;
  or?: InputMaybe<Array<BoutFilterInput>>;
  startTime?: InputMaybe<BoutFilterStartTime>;
  tags?: InputMaybe<TagFilterInput>;
};

export type BoutFilterName = {
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

export type BoutFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export const BoutSortField = {
  Category: "CATEGORY",
  Duration: "DURATION",
  EndTime: "END_TIME",
  FeedId: "FEED_ID",
  Id: "ID",
  Name: "NAME",
  StartTime: "START_TIME",
} as const;

export type BoutSortField = (typeof BoutSortField)[keyof typeof BoutSortField];
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
  audioCategory?: Maybe<AudioCategory>;
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

export type CandidateFilterAudioCategory = {
  eq?: InputMaybe<AudioCategory>;
  greaterThan?: InputMaybe<AudioCategory>;
  greaterThanOrEqual?: InputMaybe<AudioCategory>;
  in?: InputMaybe<Array<AudioCategory>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<AudioCategory>;
  lessThanOrEqual?: InputMaybe<AudioCategory>;
  notEq?: InputMaybe<AudioCategory>;
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
  audioCategory?: InputMaybe<CandidateFilterAudioCategory>;
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

export const CandidateSortField = {
  AudioCategory: "AUDIO_CATEGORY",
  Category: "CATEGORY",
  DetectionCount: "DETECTION_COUNT",
  FeedId: "FEED_ID",
  Id: "ID",
  MaxTime: "MAX_TIME",
  MinTime: "MIN_TIME",
  Visible: "VISIBLE",
} as const;

export type CandidateSortField =
  (typeof CandidateSortField)[keyof typeof CandidateSortField];
export type CandidateSortInput = {
  field: CandidateSortField;
  order?: InputMaybe<SortOrder>;
};

export type CreateBoutInput = {
  category: AudioCategory;
  endTime?: InputMaybe<Scalars["DateTime"]["input"]>;
  feedId: Scalars["String"]["input"];
  name?: InputMaybe<Scalars["String"]["input"]>;
  startTime: Scalars["DateTime"]["input"];
};

/** The result of the :create_bout mutation */
export type CreateBoutResult = {
  __typename?: "CreateBoutResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Bout>;
};

export type CreateBoutTagInput = {
  bout: ItemTagBoutTagBoutInput;
  tag: ItemTagBoutTagTagInput;
};

/** The result of the :create_bout_tag mutation */
export type CreateBoutTagResult = {
  __typename?: "CreateBoutTagResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<ItemTag>;
};

export type CreateTagInput = {
  description?: InputMaybe<Scalars["String"]["input"]>;
  name: Scalars["String"]["input"];
};

/** The result of the :create_tag mutation */
export type CreateTagResult = {
  __typename?: "CreateTagResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Tag>;
};

/** The result of the :delete_bout_tag mutation */
export type DeleteBoutTagResult = {
  __typename?: "DeleteBoutTagResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The record that was successfully deleted */
  result?: Maybe<ItemTag>;
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

export const DetectionCategory = {
  Other: "OTHER",
  Vessel: "VESSEL",
  Whale: "WHALE",
} as const;

export type DetectionCategory =
  (typeof DetectionCategory)[keyof typeof DetectionCategory];
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

export const DetectionSortField = {
  CandidateId: "CANDIDATE_ID",
  Category: "CATEGORY",
  Description: "DESCRIPTION",
  FeedId: "FEED_ID",
  Id: "ID",
  ListenerCount: "LISTENER_COUNT",
  PlayerOffset: "PLAYER_OFFSET",
  PlaylistTimestamp: "PLAYLIST_TIMESTAMP",
  SourceIp: "SOURCE_IP",
  Timestamp: "TIMESTAMP",
  Visible: "VISIBLE",
} as const;

export type DetectionSortField =
  (typeof DetectionSortField)[keyof typeof DetectionSortField];
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

export const FeedSegmentSortField = {
  Bucket: "BUCKET",
  BucketRegion: "BUCKET_REGION",
  CloudfrontUrl: "CLOUDFRONT_URL",
  Duration: "DURATION",
  EndTime: "END_TIME",
  FeedId: "FEED_ID",
  FeedStreamId: "FEED_STREAM_ID",
  FileName: "FILE_NAME",
  Id: "ID",
  PlaylistM3U8Path: "PLAYLIST_M3U8_PATH",
  PlaylistPath: "PLAYLIST_PATH",
  PlaylistTimestamp: "PLAYLIST_TIMESTAMP",
  SegmentPath: "SEGMENT_PATH",
  StartTime: "START_TIME",
} as const;

export type FeedSegmentSortField =
  (typeof FeedSegmentSortField)[keyof typeof FeedSegmentSortField];
export type FeedSegmentSortInput = {
  field: FeedSegmentSortField;
  order?: InputMaybe<SortOrder>;
};

export const FeedSortField = {
  Bucket: "BUCKET",
  BucketRegion: "BUCKET_REGION",
  CloudfrontUrl: "CLOUDFRONT_URL",
  DataplicityId: "DATAPLICITY_ID",
  Id: "ID",
  ImageUrl: "IMAGE_URL",
  IntroHtml: "INTRO_HTML",
  LocationPoint: "LOCATION_POINT",
  Name: "NAME",
  NodeName: "NODE_NAME",
  Online: "ONLINE",
  OrcahelloId: "ORCAHELLO_ID",
  Slug: "SLUG",
  Visible: "VISIBLE",
} as const;

export type FeedSortField = (typeof FeedSortField)[keyof typeof FeedSortField];
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

export const FeedStreamSortField = {
  Bucket: "BUCKET",
  BucketRegion: "BUCKET_REGION",
  CloudfrontUrl: "CLOUDFRONT_URL",
  Duration: "DURATION",
  EndTime: "END_TIME",
  FeedId: "FEED_ID",
  Id: "ID",
  NextFeedStreamId: "NEXT_FEED_STREAM_ID",
  PlaylistM3U8Path: "PLAYLIST_M3U8_PATH",
  PlaylistPath: "PLAYLIST_PATH",
  PlaylistTimestamp: "PLAYLIST_TIMESTAMP",
  PrevFeedStreamId: "PREV_FEED_STREAM_ID",
  StartTime: "START_TIME",
} as const;

export type FeedStreamSortField =
  (typeof FeedStreamSortField)[keyof typeof FeedStreamSortField];
export type FeedStreamSortInput = {
  field: FeedStreamSortField;
  order?: InputMaybe<SortOrder>;
};

export type GenerateFeedSpectrogramsInput = {
  endTime: Scalars["DateTime"]["input"];
  startTime: Scalars["DateTime"]["input"];
};

/** The result of the :generate_feed_spectrograms mutation */
export type GenerateFeedSpectrogramsResult = {
  __typename?: "GenerateFeedSpectrogramsResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Feed>;
};

export const ImageType = {
  Spectrogram: "SPECTROGRAM",
} as const;

export type ImageType = (typeof ImageType)[keyof typeof ImageType];
/** Tag applied by a user to an item: currently just bouts */
export type ItemTag = {
  __typename?: "ItemTag";
  bout?: Maybe<Bout>;
  boutId?: Maybe<Scalars["ID"]["output"]>;
  id: Scalars["ID"]["output"];
  tag?: Maybe<Tag>;
  tagId?: Maybe<Scalars["ID"]["output"]>;
  user?: Maybe<User>;
  userId?: Maybe<Scalars["ID"]["output"]>;
};

export type ItemTagBoutTagBoutInput = {
  id?: InputMaybe<Scalars["ID"]["input"]>;
};

export type ItemTagBoutTagTagInput = {
  description?: InputMaybe<Scalars["String"]["input"]>;
  id?: InputMaybe<Scalars["ID"]["input"]>;
  name?: InputMaybe<Scalars["String"]["input"]>;
};

export type ItemTagFilterBoutId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type ItemTagFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type ItemTagFilterInput = {
  and?: InputMaybe<Array<ItemTagFilterInput>>;
  bout?: InputMaybe<BoutFilterInput>;
  boutId?: InputMaybe<ItemTagFilterBoutId>;
  id?: InputMaybe<ItemTagFilterId>;
  not?: InputMaybe<Array<ItemTagFilterInput>>;
  or?: InputMaybe<Array<ItemTagFilterInput>>;
  tag?: InputMaybe<TagFilterInput>;
  tagId?: InputMaybe<ItemTagFilterTagId>;
  user?: InputMaybe<UserFilterInput>;
  userId?: InputMaybe<ItemTagFilterUserId>;
};

export type ItemTagFilterTagId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["ID"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type ItemTagFilterUserId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["ID"]["input"]>>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export const ItemTagSortField = {
  BoutId: "BOUT_ID",
  Id: "ID",
  TagId: "TAG_ID",
  UserId: "USER_ID",
} as const;

export type ItemTagSortField =
  (typeof ItemTagSortField)[keyof typeof ItemTagSortField];
export type ItemTagSortInput = {
  field: ItemTagSortField;
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

export const NotificationEventType = {
  ConfirmedCandidate: "CONFIRMED_CANDIDATE",
  LiveBout: "LIVE_BOUT",
  NewDetection: "NEW_DETECTION",
} as const;

export type NotificationEventType =
  (typeof NotificationEventType)[keyof typeof NotificationEventType];
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

export const NotificationSortField = {
  Active: "ACTIVE",
  EventType: "EVENT_TYPE",
  Finished: "FINISHED",
  Id: "ID",
  InsertedAt: "INSERTED_AT",
  NotifiedCount: "NOTIFIED_COUNT",
  NotifiedCountUpdatedAt: "NOTIFIED_COUNT_UPDATED_AT",
  Progress: "PROGRESS",
  TargetCount: "TARGET_COUNT",
} as const;

export type NotificationSortField =
  (typeof NotificationSortField)[keyof typeof NotificationSortField];
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

export type NotifyLiveBoutInput = {
  boutId: Scalars["String"]["input"];
  /**
   * What primary message subscribers will get (e.g. 'Southern Resident Killer Whales calls
   * and clicks can be heard at Orcasound Lab!')
   */
  message: Scalars["String"]["input"];
};

/** The result of the :notify_live_bout mutation */
export type NotifyLiveBoutResult = {
  __typename?: "NotifyLiveBoutResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Notification>;
};

/** A page of :audio_image */
export type PageOfAudioImage = {
  __typename?: "PageOfAudioImage";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<AudioImage>>;
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

/** A page of :item_tag */
export type PageOfItemTag = {
  __typename?: "PageOfItemTag";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<ItemTag>>;
};

/** A page of :tag */
export type PageOfTag = {
  __typename?: "PageOfTag";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<Tag>>;
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
  createBoutTag: CreateBoutTagResult;
  createTag: CreateTagResult;
  deleteBoutTag: DeleteBoutTagResult;
  generateFeedSpectrograms: GenerateFeedSpectrogramsResult;
  /** Create a notification for confirmed candidate (i.e. detection group) */
  notifyConfirmedCandidate: NotifyConfirmedCandidateResult;
  /** Create a notification for live bout */
  notifyLiveBout: NotifyLiveBoutResult;
  /** Register a new user with a username and password. */
  registerWithPassword: RegisterWithPasswordResult;
  requestPasswordReset?: Maybe<Scalars["Boolean"]["output"]>;
  resetPassword?: Maybe<PasswordResetResult>;
  setDetectionVisible: SetDetectionVisibleResult;
  signInWithPassword?: Maybe<SignInWithPasswordResult>;
  signOut?: Maybe<Scalars["Boolean"]["output"]>;
  submitDetection: SubmitDetectionResult;
  updateBout: UpdateBoutResult;
};

export type RootMutationTypeCancelCandidateNotificationsArgs = {
  id: Scalars["ID"]["input"];
  input?: InputMaybe<CancelCandidateNotificationsInput>;
};

export type RootMutationTypeCancelNotificationArgs = {
  id: Scalars["ID"]["input"];
};

export type RootMutationTypeCreateBoutArgs = {
  input: CreateBoutInput;
};

export type RootMutationTypeCreateBoutTagArgs = {
  input: CreateBoutTagInput;
};

export type RootMutationTypeCreateTagArgs = {
  input: CreateTagInput;
};

export type RootMutationTypeDeleteBoutTagArgs = {
  id: Scalars["ID"]["input"];
};

export type RootMutationTypeGenerateFeedSpectrogramsArgs = {
  id: Scalars["ID"]["input"];
  input: GenerateFeedSpectrogramsInput;
};

export type RootMutationTypeNotifyConfirmedCandidateArgs = {
  input: NotifyConfirmedCandidateInput;
};

export type RootMutationTypeNotifyLiveBoutArgs = {
  input: NotifyLiveBoutInput;
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

export type RootMutationTypeUpdateBoutArgs = {
  id: Scalars["ID"]["input"];
  input?: InputMaybe<UpdateBoutInput>;
};

export type RootQueryType = {
  __typename?: "RootQueryType";
  audioImages?: Maybe<PageOfAudioImage>;
  bout?: Maybe<Bout>;
  boutTags?: Maybe<PageOfItemTag>;
  bouts?: Maybe<PageOfBout>;
  candidate?: Maybe<Candidate>;
  candidates?: Maybe<PageOfCandidate>;
  currentUser?: Maybe<UserWithToken>;
  detection?: Maybe<Detection>;
  detections?: Maybe<PageOfDetection>;
  feed: Feed;
  feedDetectionsCount: Scalars["Int"]["output"];
  feedSegments?: Maybe<PageOfFeedSegment>;
  feedStreams?: Maybe<PageOfFeedStream>;
  feeds: Array<Feed>;
  notificationsForBout: Array<Notification>;
  notificationsForCandidate: Array<Notification>;
  searchTags: Array<Tag>;
  tags?: Maybe<PageOfTag>;
};

export type RootQueryTypeAudioImagesArgs = {
  endTime: Scalars["DateTime"]["input"];
  feedId: Scalars["String"]["input"];
  filter?: InputMaybe<AudioImageFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageSortInput>>>;
  startTime: Scalars["DateTime"]["input"];
};

export type RootQueryTypeBoutArgs = {
  id: Scalars["ID"]["input"];
};

export type RootQueryTypeBoutTagsArgs = {
  boutId: Scalars["String"]["input"];
  filter?: InputMaybe<ItemTagFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<ItemTagSortInput>>>;
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

export type RootQueryTypeDetectionArgs = {
  id: Scalars["ID"]["input"];
};

export type RootQueryTypeDetectionsArgs = {
  feedId?: InputMaybe<Scalars["String"]["input"]>;
  filter?: InputMaybe<DetectionFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<DetectionSortInput>>>;
};

export type RootQueryTypeFeedArgs = {
  filter?: InputMaybe<FeedFilterInput>;
  slug?: InputMaybe<Scalars["String"]["input"]>;
};

export type RootQueryTypeFeedDetectionsCountArgs = {
  category?: InputMaybe<DetectionCategory>;
  feedId: Scalars["String"]["input"];
  fromTime: Scalars["DateTime"]["input"];
  toTime?: InputMaybe<Scalars["DateTime"]["input"]>;
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

export type RootQueryTypeNotificationsForBoutArgs = {
  active?: InputMaybe<Scalars["Boolean"]["input"]>;
  boutId: Scalars["String"]["input"];
  eventType?: InputMaybe<NotificationEventType>;
  filter?: InputMaybe<NotificationFilterInput>;
  sort?: InputMaybe<Array<InputMaybe<NotificationSortInput>>>;
};

export type RootQueryTypeNotificationsForCandidateArgs = {
  active?: InputMaybe<Scalars["Boolean"]["input"]>;
  candidateId: Scalars["String"]["input"];
  eventType?: InputMaybe<NotificationEventType>;
  filter?: InputMaybe<NotificationFilterInput>;
  sort?: InputMaybe<Array<InputMaybe<NotificationSortInput>>>;
};

export type RootQueryTypeSearchTagsArgs = {
  filter?: InputMaybe<TagFilterInput>;
  query: Scalars["String"]["input"];
  sort?: InputMaybe<Array<InputMaybe<TagSortInput>>>;
};

export type RootQueryTypeTagsArgs = {
  filter?: InputMaybe<TagFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<TagSortInput>>>;
};

export type RootSubscriptionType = {
  __typename?: "RootSubscriptionType";
  audioImageUpdated?: Maybe<Audio_Image_Updated_Result>;
  boutNotificationSent?: Maybe<Bout_Notification_Sent_Result>;
};

export type RootSubscriptionTypeAudioImageUpdatedArgs = {
  endTime: Scalars["DateTime"]["input"];
  feedId: Scalars["String"]["input"];
  filter?: InputMaybe<AudioImageFilterInput>;
  startTime: Scalars["DateTime"]["input"];
};

export type RootSubscriptionTypeBoutNotificationSentArgs = {
  active?: InputMaybe<Scalars["Boolean"]["input"]>;
  boutId: Scalars["String"]["input"];
  eventType?: InputMaybe<NotificationEventType>;
  filter?: InputMaybe<NotificationFilterInput>;
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

export const SortOrder = {
  Asc: "ASC",
  AscNullsFirst: "ASC_NULLS_FIRST",
  AscNullsLast: "ASC_NULLS_LAST",
  Desc: "DESC",
  DescNullsFirst: "DESC_NULLS_FIRST",
  DescNullsLast: "DESC_NULLS_LAST",
} as const;

export type SortOrder = (typeof SortOrder)[keyof typeof SortOrder];
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

/** Tag definition with a name, description, and unique slug */
export type Tag = {
  __typename?: "Tag";
  description?: Maybe<Scalars["String"]["output"]>;
  id: Scalars["ID"]["output"];
  name: Scalars["String"]["output"];
  slug: Scalars["String"]["output"];
};

export type TagFilterDescription = {
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

export type TagFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type TagFilterInput = {
  and?: InputMaybe<Array<TagFilterInput>>;
  description?: InputMaybe<TagFilterDescription>;
  id?: InputMaybe<TagFilterId>;
  name?: InputMaybe<TagFilterName>;
  not?: InputMaybe<Array<TagFilterInput>>;
  or?: InputMaybe<Array<TagFilterInput>>;
  slug?: InputMaybe<TagFilterSlug>;
};

export type TagFilterName = {
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

export type TagFilterSlug = {
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

export const TagSortField = {
  Description: "DESCRIPTION",
  Id: "ID",
  Name: "NAME",
  Slug: "SLUG",
} as const;

export type TagSortField = (typeof TagSortField)[keyof typeof TagSortField];
export type TagSortInput = {
  field: TagSortField;
  order?: InputMaybe<SortOrder>;
};

export type UpdateBoutInput = {
  category?: InputMaybe<AudioCategory>;
  endTime?: InputMaybe<Scalars["DateTime"]["input"]>;
  name?: InputMaybe<Scalars["String"]["input"]>;
  startTime?: InputMaybe<Scalars["DateTime"]["input"]>;
};

/** The result of the :update_bout mutation */
export type UpdateBoutResult = {
  __typename?: "UpdateBoutResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Bout>;
};

export type User = {
  __typename?: "User";
  admin?: Maybe<Scalars["Boolean"]["output"]>;
  email?: Maybe<Scalars["String"]["output"]>;
  firstName?: Maybe<Scalars["String"]["output"]>;
  id: Scalars["ID"]["output"];
  lastName?: Maybe<Scalars["String"]["output"]>;
  moderator?: Maybe<Scalars["Boolean"]["output"]>;
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

export type UserWithToken = {
  __typename?: "UserWithToken";
  admin?: Maybe<Scalars["Boolean"]["output"]>;
  email?: Maybe<Scalars["String"]["output"]>;
  firstName?: Maybe<Scalars["String"]["output"]>;
  id: Scalars["ID"]["output"];
  lastName?: Maybe<Scalars["String"]["output"]>;
  moderator?: Maybe<Scalars["Boolean"]["output"]>;
  token?: Maybe<Scalars["String"]["output"]>;
  username?: Maybe<Scalars["String"]["output"]>;
};

export type Audio_Image_Updated_Result = {
  __typename?: "audio_image_updated_result";
  created?: Maybe<AudioImage>;
  updated?: Maybe<AudioImage>;
};

export type Bout_Notification_Sent_Result = {
  __typename?: "bout_notification_sent_result";
  updated?: Maybe<Notification>;
};

export type AudioImagePartsFragment = {
  __typename?: "AudioImage";
  id: string;
  startTime: Date;
  endTime: Date;
  status: string;
  objectPath?: string | null;
  bucket?: string | null;
  bucketRegion?: string | null;
  feedId: string;
  imageSize?: number | null;
  imageType?: ImageType | null;
};

export type BoutPartsFragment = {
  __typename?: "Bout";
  id: string;
  name?: string | null;
  category: AudioCategory;
  duration?: number | null;
  endTime?: Date | null;
  startTime: Date;
};

export type ErrorPartsFragment = {
  __typename?: "MutationError";
  code?: string | null;
  fields?: Array<string> | null;
  message?: string | null;
  shortMessage?: string | null;
  vars?: { [key: string]: any } | null;
};

export type FeedPartsFragment = {
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

export type FeedSegmentPartsFragment = {
  __typename?: "FeedSegment";
  id: string;
  startTime?: Date | null;
  endTime?: Date | null;
  duration?: number | null;
  bucket?: string | null;
  bucketRegion?: string | null;
  cloudfrontUrl?: string | null;
  fileName: string;
  playlistM3u8Path?: string | null;
  playlistPath?: string | null;
  playlistTimestamp?: string | null;
  segmentPath?: string | null;
};

export type FeedStreamPartsFragment = {
  __typename?: "FeedStream";
  id: string;
  startTime?: Date | null;
  endTime?: Date | null;
  duration?: number | null;
  bucket?: string | null;
  bucketRegion?: string | null;
  cloudfrontUrl?: string | null;
  playlistTimestamp?: string | null;
  playlistPath?: string | null;
  playlistM3u8Path?: string | null;
};

export type ItemTagPartsFragment = {
  __typename?: "ItemTag";
  id: string;
  user?: { __typename?: "User"; username?: string | null } | null;
  tag?: {
    __typename?: "Tag";
    id: string;
    name: string;
    slug: string;
    description?: string | null;
  } | null;
};

export type NotificationPartsFragment = {
  __typename?: "Notification";
  id: string;
  active?: boolean | null;
  eventType?: NotificationEventType | null;
  progress?: number | null;
  targetCount?: number | null;
  finished?: boolean | null;
  notifiedCount?: number | null;
  notifiedCountUpdatedAt?: Date | null;
  insertedAt: Date;
};

export type TagPartsFragment = {
  __typename?: "Tag";
  id: string;
  name: string;
  description?: string | null;
  slug: string;
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

export type CreateBoutMutationVariables = Exact<{
  feedId: Scalars["String"]["input"];
  name?: InputMaybe<Scalars["String"]["input"]>;
  startTime: Scalars["DateTime"]["input"];
  endTime?: InputMaybe<Scalars["DateTime"]["input"]>;
  category: AudioCategory;
}>;

export type CreateBoutMutation = {
  __typename?: "RootMutationType";
  createBout: {
    __typename?: "CreateBoutResult";
    result?: {
      __typename?: "Bout";
      id: string;
      name?: string | null;
      category: AudioCategory;
      duration?: number | null;
      endTime?: Date | null;
      startTime: Date;
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

export type CreateBoutTagMutationVariables = Exact<{
  tagId?: InputMaybe<Scalars["ID"]["input"]>;
  tagName: Scalars["String"]["input"];
  tagDescription?: InputMaybe<Scalars["String"]["input"]>;
  boutId: Scalars["ID"]["input"];
}>;

export type CreateBoutTagMutation = {
  __typename?: "RootMutationType";
  createBoutTag: {
    __typename?: "CreateBoutTagResult";
    result?: {
      __typename?: "ItemTag";
      id: string;
      user?: { __typename?: "User"; username?: string | null } | null;
      tag?: {
        __typename?: "Tag";
        id: string;
        name: string;
        slug: string;
        description?: string | null;
      } | null;
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

export type DeleteBoutTagMutationVariables = Exact<{
  boutTagId: Scalars["ID"]["input"];
}>;

export type DeleteBoutTagMutation = {
  __typename?: "RootMutationType";
  deleteBoutTag: {
    __typename?: "DeleteBoutTagResult";
    result?: {
      __typename?: "ItemTag";
      id: string;
      user?: { __typename?: "User"; username?: string | null } | null;
      tag?: {
        __typename?: "Tag";
        id: string;
        name: string;
        slug: string;
        description?: string | null;
      } | null;
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

export type GenerateFeedSpectrogramsMutationVariables = Exact<{
  feedId: Scalars["ID"]["input"];
  startTime: Scalars["DateTime"]["input"];
  endTime: Scalars["DateTime"]["input"];
}>;

export type GenerateFeedSpectrogramsMutation = {
  __typename?: "RootMutationType";
  generateFeedSpectrograms: {
    __typename?: "GenerateFeedSpectrogramsResult";
    result?: {
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

export type NotifyLiveBoutMutationVariables = Exact<{
  boutId: Scalars["String"]["input"];
  message: Scalars["String"]["input"];
}>;

export type NotifyLiveBoutMutation = {
  __typename?: "RootMutationType";
  notifyLiveBout: {
    __typename?: "NotifyLiveBoutResult";
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
      email?: string | null;
      username?: string | null;
      admin?: boolean | null;
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
      email?: string | null;
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
      email?: string | null;
      admin?: boolean | null;
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

export type UpdateBoutMutationVariables = Exact<{
  id: Scalars["ID"]["input"];
  startTime: Scalars["DateTime"]["input"];
  endTime?: InputMaybe<Scalars["DateTime"]["input"]>;
  name?: InputMaybe<Scalars["String"]["input"]>;
  category: AudioCategory;
}>;

export type UpdateBoutMutation = {
  __typename?: "RootMutationType";
  updateBout: {
    __typename?: "UpdateBoutResult";
    result?: {
      __typename?: "Bout";
      id: string;
      name?: string | null;
      category: AudioCategory;
      duration?: number | null;
      endTime?: Date | null;
      startTime: Date;
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

export type BoutQueryVariables = Exact<{
  id: Scalars["ID"]["input"];
}>;

export type BoutQuery = {
  __typename?: "RootQueryType";
  bout?: {
    __typename?: "Bout";
    id: string;
    name?: string | null;
    category: AudioCategory;
    duration?: number | null;
    endTime?: Date | null;
    startTime: Date;
    feed?: {
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
    } | null;
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
    __typename?: "UserWithToken";
    id: string;
    firstName?: string | null;
    lastName?: string | null;
    username?: string | null;
    email?: string | null;
    admin?: boolean | null;
    moderator?: boolean | null;
    token?: string | null;
  } | null;
};

export type DetectionsCountQueryVariables = Exact<{
  feedId: Scalars["String"]["input"];
  fromTime: Scalars["DateTime"]["input"];
  toTime?: InputMaybe<Scalars["DateTime"]["input"]>;
  category?: InputMaybe<DetectionCategory>;
}>;

export type DetectionsCountQuery = {
  __typename?: "RootQueryType";
  feedDetectionsCount: number;
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

export type AudioImagesQueryVariables = Exact<{
  feedId: Scalars["String"]["input"];
  startTime: Scalars["DateTime"]["input"];
  endTime: Scalars["DateTime"]["input"];
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
}>;

export type AudioImagesQuery = {
  __typename?: "RootQueryType";
  audioImages?: {
    __typename?: "PageOfAudioImage";
    hasNextPage: boolean;
    results?: Array<{
      __typename?: "AudioImage";
      id: string;
      startTime: Date;
      endTime: Date;
      status: string;
      objectPath?: string | null;
      bucket?: string | null;
      bucketRegion?: string | null;
      feedId: string;
      imageSize?: number | null;
      imageType?: ImageType | null;
    }> | null;
  } | null;
};

export type BoutTagsQueryVariables = Exact<{
  boutId: Scalars["String"]["input"];
}>;

export type BoutTagsQuery = {
  __typename?: "RootQueryType";
  boutTags?: {
    __typename?: "PageOfItemTag";
    count?: number | null;
    results?: Array<{
      __typename?: "ItemTag";
      id: string;
      user?: { __typename?: "User"; username?: string | null } | null;
      tag?: {
        __typename?: "Tag";
        id: string;
        name: string;
        slug: string;
        description?: string | null;
      } | null;
    }> | null;
  } | null;
};

export type BoutsQueryVariables = Exact<{
  feedId?: InputMaybe<Scalars["String"]["input"]>;
  filter?: InputMaybe<BoutFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<
    Array<InputMaybe<BoutSortInput>> | InputMaybe<BoutSortInput>
  >;
}>;

export type BoutsQuery = {
  __typename?: "RootQueryType";
  bouts?: {
    __typename?: "PageOfBout";
    count?: number | null;
    hasNextPage: boolean;
    results?: Array<{
      __typename?: "Bout";
      id: string;
      name?: string | null;
      category: AudioCategory;
      duration?: number | null;
      endTime?: Date | null;
      startTime: Date;
      feed?: {
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
      } | null;
    }> | null;
  } | null;
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
  feedId?: InputMaybe<Scalars["String"]["input"]>;
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

export type ListFeedStreamsQueryVariables = Exact<{
  feedId?: InputMaybe<Scalars["String"]["input"]>;
  fromDateTime: Scalars["DateTime"]["input"];
  toDateTime: Scalars["DateTime"]["input"];
  dayBeforeFromDateTime: Scalars["DateTime"]["input"];
}>;

export type ListFeedStreamsQuery = {
  __typename?: "RootQueryType";
  feedStreams?: {
    __typename?: "PageOfFeedStream";
    count?: number | null;
    results?: Array<{
      __typename?: "FeedStream";
      id: string;
      startTime?: Date | null;
      endTime?: Date | null;
      duration?: number | null;
      bucket?: string | null;
      bucketRegion?: string | null;
      cloudfrontUrl?: string | null;
      playlistTimestamp?: string | null;
      playlistPath?: string | null;
      playlistM3u8Path?: string | null;
      feedSegments: Array<{
        __typename?: "FeedSegment";
        id: string;
        startTime?: Date | null;
        endTime?: Date | null;
        duration?: number | null;
        bucket?: string | null;
        bucketRegion?: string | null;
        cloudfrontUrl?: string | null;
        fileName: string;
        playlistM3u8Path?: string | null;
        playlistPath?: string | null;
        playlistTimestamp?: string | null;
        segmentPath?: string | null;
      }>;
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
    mapUrl?: string | null;
    bucket: string;
    online?: boolean | null;
    latLng: { __typename?: "LatLng"; lat: number; lng: number };
  }>;
};

export type NotificationsForBoutQueryVariables = Exact<{
  boutId: Scalars["String"]["input"];
  eventType?: InputMaybe<NotificationEventType>;
}>;

export type NotificationsForBoutQuery = {
  __typename?: "RootQueryType";
  notificationsForBout: Array<{
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

export type TagsQueryVariables = Exact<{
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  filter?: InputMaybe<TagFilterInput>;
  sort?: InputMaybe<Array<InputMaybe<TagSortInput>> | InputMaybe<TagSortInput>>;
}>;

export type TagsQuery = {
  __typename?: "RootQueryType";
  tags?: {
    __typename?: "PageOfTag";
    count?: number | null;
    hasNextPage: boolean;
    results?: Array<{
      __typename?: "Tag";
      id: string;
      name: string;
      description?: string | null;
      slug: string;
    }> | null;
  } | null;
};

export type SearchTagsQueryVariables = Exact<{
  query: Scalars["String"]["input"];
}>;

export type SearchTagsQuery = {
  __typename?: "RootQueryType";
  searchTags: Array<{
    __typename?: "Tag";
    id: string;
    name: string;
    description?: string | null;
    slug: string;
  }>;
};

export type AudioImageUpdatedSubscriptionVariables = Exact<{
  feedId: Scalars["String"]["input"];
  startTime: Scalars["DateTime"]["input"];
  endTime: Scalars["DateTime"]["input"];
}>;

export type AudioImageUpdatedSubscription = {
  __typename?: "RootSubscriptionType";
  audioImageUpdated?: {
    __typename?: "audio_image_updated_result";
    created?: {
      __typename?: "AudioImage";
      id: string;
      startTime: Date;
      endTime: Date;
      status: string;
      objectPath?: string | null;
      bucket?: string | null;
      bucketRegion?: string | null;
      feedId: string;
      imageSize?: number | null;
      imageType?: ImageType | null;
    } | null;
    updated?: {
      __typename?: "AudioImage";
      id: string;
      startTime: Date;
      endTime: Date;
      status: string;
      objectPath?: string | null;
      bucket?: string | null;
      bucketRegion?: string | null;
      feedId: string;
      imageSize?: number | null;
      imageType?: ImageType | null;
    } | null;
  } | null;
};

export type BoutNotificationSentSubscriptionVariables = Exact<{
  boutId: Scalars["String"]["input"];
}>;

export type BoutNotificationSentSubscription = {
  __typename?: "RootSubscriptionType";
  boutNotificationSent?: {
    __typename?: "bout_notification_sent_result";
    updated?: {
      __typename?: "Notification";
      id: string;
      active?: boolean | null;
      eventType?: NotificationEventType | null;
      progress?: number | null;
      targetCount?: number | null;
      finished?: boolean | null;
      notifiedCount?: number | null;
      notifiedCountUpdatedAt?: Date | null;
      insertedAt: Date;
    } | null;
  } | null;
};

export const AudioImagePartsFragmentDoc = `
    fragment AudioImageParts on AudioImage {
  id
  startTime
  endTime
  status
  objectPath
  bucket
  bucketRegion
  feedId
  imageSize
  imageType
}
    `;
export const BoutPartsFragmentDoc = `
    fragment BoutParts on Bout {
  id
  name
  category
  duration
  endTime
  startTime
}
    `;
export const ErrorPartsFragmentDoc = `
    fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}
    `;
export const FeedPartsFragmentDoc = `
    fragment FeedParts on Feed {
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
    `;
export const FeedSegmentPartsFragmentDoc = `
    fragment FeedSegmentParts on FeedSegment {
  id
  startTime
  endTime
  duration
  bucket
  bucketRegion
  cloudfrontUrl
  fileName
  playlistM3u8Path
  playlistPath
  playlistTimestamp
  segmentPath
}
    `;
export const FeedStreamPartsFragmentDoc = `
    fragment FeedStreamParts on FeedStream {
  id
  startTime
  endTime
  duration
  bucket
  bucketRegion
  cloudfrontUrl
  playlistTimestamp
  playlistPath
  playlistM3u8Path
}
    `;
export const ItemTagPartsFragmentDoc = `
    fragment ItemTagParts on ItemTag {
  id
  user {
    username
  }
  tag {
    id
    name
    slug
    description
  }
}
    `;
export const NotificationPartsFragmentDoc = `
    fragment NotificationParts on Notification {
  id
  active
  eventType
  progress
  targetCount
  finished
  notifiedCount
  notifiedCountUpdatedAt
  insertedAt
}
    `;
export const TagPartsFragmentDoc = `
    fragment TagParts on Tag {
  id
  name
  description
  slug
}
    `;
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

export const CreateBoutDocument = `
    mutation createBout($feedId: String!, $name: String, $startTime: DateTime!, $endTime: DateTime, $category: AudioCategory!) {
  createBout(
    input: {feedId: $feedId, category: $category, startTime: $startTime, endTime: $endTime, name: $name}
  ) {
    result {
      ...BoutParts
    }
    errors {
      ...ErrorParts
    }
  }
}
    ${BoutPartsFragmentDoc}
${ErrorPartsFragmentDoc}`;

export const useCreateBoutMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    CreateBoutMutation,
    TError,
    CreateBoutMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    CreateBoutMutation,
    TError,
    CreateBoutMutationVariables,
    TContext
  >({
    mutationKey: ["createBout"],
    mutationFn: (variables?: CreateBoutMutationVariables) =>
      fetcher<CreateBoutMutation, CreateBoutMutationVariables>(
        CreateBoutDocument,
        variables,
      )(),
    ...options,
  });
};

useCreateBoutMutation.getKey = () => ["createBout"];

useCreateBoutMutation.fetcher = (
  variables: CreateBoutMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<CreateBoutMutation, CreateBoutMutationVariables>(
    CreateBoutDocument,
    variables,
    options,
  );

export const CreateBoutTagDocument = `
    mutation createBoutTag($tagId: ID, $tagName: String!, $tagDescription: String, $boutId: ID!) {
  createBoutTag(
    input: {bout: {id: $boutId}, tag: {id: $tagId, name: $tagName, description: $tagDescription}}
  ) {
    result {
      ...ItemTagParts
    }
    errors {
      ...ErrorParts
    }
  }
}
    ${ItemTagPartsFragmentDoc}
${ErrorPartsFragmentDoc}`;

export const useCreateBoutTagMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    CreateBoutTagMutation,
    TError,
    CreateBoutTagMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    CreateBoutTagMutation,
    TError,
    CreateBoutTagMutationVariables,
    TContext
  >({
    mutationKey: ["createBoutTag"],
    mutationFn: (variables?: CreateBoutTagMutationVariables) =>
      fetcher<CreateBoutTagMutation, CreateBoutTagMutationVariables>(
        CreateBoutTagDocument,
        variables,
      )(),
    ...options,
  });
};

useCreateBoutTagMutation.getKey = () => ["createBoutTag"];

useCreateBoutTagMutation.fetcher = (
  variables: CreateBoutTagMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<CreateBoutTagMutation, CreateBoutTagMutationVariables>(
    CreateBoutTagDocument,
    variables,
    options,
  );

export const DeleteBoutTagDocument = `
    mutation deleteBoutTag($boutTagId: ID!) {
  deleteBoutTag(id: $boutTagId) {
    result {
      ...ItemTagParts
    }
    errors {
      ...ErrorParts
    }
  }
}
    ${ItemTagPartsFragmentDoc}
${ErrorPartsFragmentDoc}`;

export const useDeleteBoutTagMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    DeleteBoutTagMutation,
    TError,
    DeleteBoutTagMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    DeleteBoutTagMutation,
    TError,
    DeleteBoutTagMutationVariables,
    TContext
  >({
    mutationKey: ["deleteBoutTag"],
    mutationFn: (variables?: DeleteBoutTagMutationVariables) =>
      fetcher<DeleteBoutTagMutation, DeleteBoutTagMutationVariables>(
        DeleteBoutTagDocument,
        variables,
      )(),
    ...options,
  });
};

useDeleteBoutTagMutation.getKey = () => ["deleteBoutTag"];

useDeleteBoutTagMutation.fetcher = (
  variables: DeleteBoutTagMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<DeleteBoutTagMutation, DeleteBoutTagMutationVariables>(
    DeleteBoutTagDocument,
    variables,
    options,
  );

export const GenerateFeedSpectrogramsDocument = `
    mutation generateFeedSpectrograms($feedId: ID!, $startTime: DateTime!, $endTime: DateTime!) {
  generateFeedSpectrograms(
    id: $feedId
    input: {startTime: $startTime, endTime: $endTime}
  ) {
    result {
      ...FeedParts
    }
    errors {
      ...ErrorParts
    }
  }
}
    ${FeedPartsFragmentDoc}
${ErrorPartsFragmentDoc}`;

export const useGenerateFeedSpectrogramsMutation = <
  TError = unknown,
  TContext = unknown,
>(
  options?: UseMutationOptions<
    GenerateFeedSpectrogramsMutation,
    TError,
    GenerateFeedSpectrogramsMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    GenerateFeedSpectrogramsMutation,
    TError,
    GenerateFeedSpectrogramsMutationVariables,
    TContext
  >({
    mutationKey: ["generateFeedSpectrograms"],
    mutationFn: (variables?: GenerateFeedSpectrogramsMutationVariables) =>
      fetcher<
        GenerateFeedSpectrogramsMutation,
        GenerateFeedSpectrogramsMutationVariables
      >(GenerateFeedSpectrogramsDocument, variables)(),
    ...options,
  });
};

useGenerateFeedSpectrogramsMutation.getKey = () => ["generateFeedSpectrograms"];

useGenerateFeedSpectrogramsMutation.fetcher = (
  variables: GenerateFeedSpectrogramsMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<
    GenerateFeedSpectrogramsMutation,
    GenerateFeedSpectrogramsMutationVariables
  >(GenerateFeedSpectrogramsDocument, variables, options);

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

export const NotifyLiveBoutDocument = `
    mutation notifyLiveBout($boutId: String!, $message: String!) {
  notifyLiveBout(input: {boutId: $boutId, message: $message}) {
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

export const useNotifyLiveBoutMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    NotifyLiveBoutMutation,
    TError,
    NotifyLiveBoutMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    NotifyLiveBoutMutation,
    TError,
    NotifyLiveBoutMutationVariables,
    TContext
  >({
    mutationKey: ["notifyLiveBout"],
    mutationFn: (variables?: NotifyLiveBoutMutationVariables) =>
      fetcher<NotifyLiveBoutMutation, NotifyLiveBoutMutationVariables>(
        NotifyLiveBoutDocument,
        variables,
      )(),
    ...options,
  });
};

useNotifyLiveBoutMutation.getKey = () => ["notifyLiveBout"];

useNotifyLiveBoutMutation.fetcher = (
  variables: NotifyLiveBoutMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<NotifyLiveBoutMutation, NotifyLiveBoutMutationVariables>(
    NotifyLiveBoutDocument,
    variables,
    options,
  );

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

export const UpdateBoutDocument = `
    mutation updateBout($id: ID!, $startTime: DateTime!, $endTime: DateTime, $name: String, $category: AudioCategory!) {
  updateBout(
    id: $id
    input: {category: $category, startTime: $startTime, endTime: $endTime, name: $name}
  ) {
    result {
      ...BoutParts
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
    ${BoutPartsFragmentDoc}`;

export const useUpdateBoutMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    UpdateBoutMutation,
    TError,
    UpdateBoutMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    UpdateBoutMutation,
    TError,
    UpdateBoutMutationVariables,
    TContext
  >({
    mutationKey: ["updateBout"],
    mutationFn: (variables?: UpdateBoutMutationVariables) =>
      fetcher<UpdateBoutMutation, UpdateBoutMutationVariables>(
        UpdateBoutDocument,
        variables,
      )(),
    ...options,
  });
};

useUpdateBoutMutation.getKey = () => ["updateBout"];

useUpdateBoutMutation.fetcher = (
  variables: UpdateBoutMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<UpdateBoutMutation, UpdateBoutMutationVariables>(
    UpdateBoutDocument,
    variables,
    options,
  );

export const BoutDocument = `
    query bout($id: ID!) {
  bout(id: $id) {
    ...BoutParts
    feed {
      ...FeedParts
    }
  }
}
    ${BoutPartsFragmentDoc}
${FeedPartsFragmentDoc}`;

export const useBoutQuery = <TData = BoutQuery, TError = unknown>(
  variables: BoutQueryVariables,
  options?: Omit<UseQueryOptions<BoutQuery, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<BoutQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<BoutQuery, TError, TData>({
    queryKey: ["bout", variables],
    queryFn: fetcher<BoutQuery, BoutQueryVariables>(BoutDocument, variables),
    ...options,
  });
};

useBoutQuery.document = BoutDocument;

useBoutQuery.getKey = (variables: BoutQueryVariables) => ["bout", variables];

useBoutQuery.fetcher = (
  variables: BoutQueryVariables,
  options?: RequestInit["headers"],
) => fetcher<BoutQuery, BoutQueryVariables>(BoutDocument, variables, options);

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
    username
    email
    admin
    moderator
    token
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

export const DetectionsCountDocument = `
    query detectionsCount($feedId: String!, $fromTime: DateTime!, $toTime: DateTime, $category: DetectionCategory) {
  feedDetectionsCount(
    feedId: $feedId
    fromTime: $fromTime
    toTime: $toTime
    category: $category
  )
}
    `;

export const useDetectionsCountQuery = <
  TData = DetectionsCountQuery,
  TError = unknown,
>(
  variables: DetectionsCountQueryVariables,
  options?: Omit<
    UseQueryOptions<DetectionsCountQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<DetectionsCountQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<DetectionsCountQuery, TError, TData>({
    queryKey: ["detectionsCount", variables],
    queryFn: fetcher<DetectionsCountQuery, DetectionsCountQueryVariables>(
      DetectionsCountDocument,
      variables,
    ),
    ...options,
  });
};

useDetectionsCountQuery.document = DetectionsCountDocument;

useDetectionsCountQuery.getKey = (variables: DetectionsCountQueryVariables) => [
  "detectionsCount",
  variables,
];

useDetectionsCountQuery.fetcher = (
  variables: DetectionsCountQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<DetectionsCountQuery, DetectionsCountQueryVariables>(
    DetectionsCountDocument,
    variables,
    options,
  );

export const FeedDocument = `
    query feed($slug: String!) {
  feed(slug: $slug) {
    ...FeedParts
  }
}
    ${FeedPartsFragmentDoc}`;

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

export const AudioImagesDocument = `
    query audioImages($feedId: String!, $startTime: DateTime!, $endTime: DateTime!, $limit: Int = 1000, $offset: Int = 0) {
  audioImages(
    feedId: $feedId
    startTime: $startTime
    endTime: $endTime
    filter: {status: {notEq: "FAILED"}}
    limit: $limit
    offset: $offset
  ) {
    hasNextPage
    results {
      ...AudioImageParts
    }
  }
}
    ${AudioImagePartsFragmentDoc}`;

export const useAudioImagesQuery = <TData = AudioImagesQuery, TError = unknown>(
  variables: AudioImagesQueryVariables,
  options?: Omit<
    UseQueryOptions<AudioImagesQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<AudioImagesQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<AudioImagesQuery, TError, TData>({
    queryKey: ["audioImages", variables],
    queryFn: fetcher<AudioImagesQuery, AudioImagesQueryVariables>(
      AudioImagesDocument,
      variables,
    ),
    ...options,
  });
};

useAudioImagesQuery.document = AudioImagesDocument;

useAudioImagesQuery.getKey = (variables: AudioImagesQueryVariables) => [
  "audioImages",
  variables,
];

useAudioImagesQuery.fetcher = (
  variables: AudioImagesQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<AudioImagesQuery, AudioImagesQueryVariables>(
    AudioImagesDocument,
    variables,
    options,
  );

export const BoutTagsDocument = `
    query boutTags($boutId: String!) {
  boutTags(boutId: $boutId) {
    count
    results {
      ...ItemTagParts
    }
  }
}
    ${ItemTagPartsFragmentDoc}`;

export const useBoutTagsQuery = <TData = BoutTagsQuery, TError = unknown>(
  variables: BoutTagsQueryVariables,
  options?: Omit<UseQueryOptions<BoutTagsQuery, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<BoutTagsQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<BoutTagsQuery, TError, TData>({
    queryKey: ["boutTags", variables],
    queryFn: fetcher<BoutTagsQuery, BoutTagsQueryVariables>(
      BoutTagsDocument,
      variables,
    ),
    ...options,
  });
};

useBoutTagsQuery.document = BoutTagsDocument;

useBoutTagsQuery.getKey = (variables: BoutTagsQueryVariables) => [
  "boutTags",
  variables,
];

useBoutTagsQuery.fetcher = (
  variables: BoutTagsQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<BoutTagsQuery, BoutTagsQueryVariables>(
    BoutTagsDocument,
    variables,
    options,
  );

export const BoutsDocument = `
    query bouts($feedId: String, $filter: BoutFilterInput, $limit: Int = 100, $offset: Int, $sort: [BoutSortInput]) {
  bouts(
    feedId: $feedId
    filter: $filter
    limit: $limit
    offset: $offset
    sort: $sort
  ) {
    count
    hasNextPage
    results {
      ...BoutParts
      feed {
        ...FeedParts
      }
    }
  }
}
    ${BoutPartsFragmentDoc}
${FeedPartsFragmentDoc}`;

export const useBoutsQuery = <TData = BoutsQuery, TError = unknown>(
  variables?: BoutsQueryVariables,
  options?: Omit<UseQueryOptions<BoutsQuery, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<BoutsQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<BoutsQuery, TError, TData>({
    queryKey: variables === undefined ? ["bouts"] : ["bouts", variables],
    queryFn: fetcher<BoutsQuery, BoutsQueryVariables>(BoutsDocument, variables),
    ...options,
  });
};

useBoutsQuery.document = BoutsDocument;

useBoutsQuery.getKey = (variables?: BoutsQueryVariables) =>
  variables === undefined ? ["bouts"] : ["bouts", variables];

useBoutsQuery.fetcher = (
  variables?: BoutsQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<BoutsQuery, BoutsQueryVariables>(BoutsDocument, variables, options);

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
    query detections($feedId: String, $filter: DetectionFilterInput, $limit: Int, $offset: Int, $sort: [DetectionSortInput]) {
  detections(
    feedId: $feedId
    filter: $filter
    limit: $limit
    offset: $offset
    sort: $sort
  ) {
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

export const ListFeedStreamsDocument = `
    query listFeedStreams($feedId: String, $fromDateTime: DateTime!, $toDateTime: DateTime!, $dayBeforeFromDateTime: DateTime!) {
  feedStreams(
    feedId: $feedId
    filter: {and: [{startTime: {lessThanOrEqual: $toDateTime}}, {startTime: {greaterThanOrEqual: $dayBeforeFromDateTime}}], or: [{endTime: {isNil: true}}, {endTime: {greaterThanOrEqual: $fromDateTime}}]}
    sort: {field: START_TIME, order: DESC}
    limit: 2
  ) {
    count
    results {
      ...FeedStreamParts
      feedSegments(
        filter: {and: [{startTime: {lessThanOrEqual: $toDateTime}}, {startTime: {greaterThanOrEqual: $dayBeforeFromDateTime}}], endTime: {greaterThanOrEqual: $fromDateTime}}
        sort: {field: START_TIME, order: ASC}
      ) {
        ...FeedSegmentParts
      }
    }
  }
}
    ${FeedStreamPartsFragmentDoc}
${FeedSegmentPartsFragmentDoc}`;

export const useListFeedStreamsQuery = <
  TData = ListFeedStreamsQuery,
  TError = unknown,
>(
  variables: ListFeedStreamsQueryVariables,
  options?: Omit<
    UseQueryOptions<ListFeedStreamsQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<ListFeedStreamsQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<ListFeedStreamsQuery, TError, TData>({
    queryKey: ["listFeedStreams", variables],
    queryFn: fetcher<ListFeedStreamsQuery, ListFeedStreamsQueryVariables>(
      ListFeedStreamsDocument,
      variables,
    ),
    ...options,
  });
};

useListFeedStreamsQuery.document = ListFeedStreamsDocument;

useListFeedStreamsQuery.getKey = (variables: ListFeedStreamsQueryVariables) => [
  "listFeedStreams",
  variables,
];

useListFeedStreamsQuery.fetcher = (
  variables: ListFeedStreamsQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<ListFeedStreamsQuery, ListFeedStreamsQueryVariables>(
    ListFeedStreamsDocument,
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
    mapUrl
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

export const NotificationsForBoutDocument = `
    query notificationsForBout($boutId: String!, $eventType: NotificationEventType) {
  notificationsForBout(boutId: $boutId, eventType: $eventType) {
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

export const useNotificationsForBoutQuery = <
  TData = NotificationsForBoutQuery,
  TError = unknown,
>(
  variables: NotificationsForBoutQueryVariables,
  options?: Omit<
    UseQueryOptions<NotificationsForBoutQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<
      NotificationsForBoutQuery,
      TError,
      TData
    >["queryKey"];
  },
) => {
  return useQuery<NotificationsForBoutQuery, TError, TData>({
    queryKey: ["notificationsForBout", variables],
    queryFn: fetcher<
      NotificationsForBoutQuery,
      NotificationsForBoutQueryVariables
    >(NotificationsForBoutDocument, variables),
    ...options,
  });
};

useNotificationsForBoutQuery.document = NotificationsForBoutDocument;

useNotificationsForBoutQuery.getKey = (
  variables: NotificationsForBoutQueryVariables,
) => ["notificationsForBout", variables];

useNotificationsForBoutQuery.fetcher = (
  variables: NotificationsForBoutQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<NotificationsForBoutQuery, NotificationsForBoutQueryVariables>(
    NotificationsForBoutDocument,
    variables,
    options,
  );

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

export const TagsDocument = `
    query tags($limit: Int, $offset: Int, $filter: TagFilterInput, $sort: [TagSortInput]) {
  tags(limit: $limit, offset: $offset, filter: $filter, sort: $sort) {
    count
    hasNextPage
    results {
      ...TagParts
    }
  }
}
    ${TagPartsFragmentDoc}`;

export const useTagsQuery = <TData = TagsQuery, TError = unknown>(
  variables?: TagsQueryVariables,
  options?: Omit<UseQueryOptions<TagsQuery, TError, TData>, "queryKey"> & {
    queryKey?: UseQueryOptions<TagsQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<TagsQuery, TError, TData>({
    queryKey: variables === undefined ? ["tags"] : ["tags", variables],
    queryFn: fetcher<TagsQuery, TagsQueryVariables>(TagsDocument, variables),
    ...options,
  });
};

useTagsQuery.document = TagsDocument;

useTagsQuery.getKey = (variables?: TagsQueryVariables) =>
  variables === undefined ? ["tags"] : ["tags", variables];

useTagsQuery.fetcher = (
  variables?: TagsQueryVariables,
  options?: RequestInit["headers"],
) => fetcher<TagsQuery, TagsQueryVariables>(TagsDocument, variables, options);

export const SearchTagsDocument = `
    query searchTags($query: String!) {
  searchTags(query: $query) {
    ...TagParts
  }
}
    ${TagPartsFragmentDoc}`;

export const useSearchTagsQuery = <TData = SearchTagsQuery, TError = unknown>(
  variables: SearchTagsQueryVariables,
  options?: Omit<
    UseQueryOptions<SearchTagsQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<SearchTagsQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<SearchTagsQuery, TError, TData>({
    queryKey: ["searchTags", variables],
    queryFn: fetcher<SearchTagsQuery, SearchTagsQueryVariables>(
      SearchTagsDocument,
      variables,
    ),
    ...options,
  });
};

useSearchTagsQuery.document = SearchTagsDocument;

useSearchTagsQuery.getKey = (variables: SearchTagsQueryVariables) => [
  "searchTags",
  variables,
];

useSearchTagsQuery.fetcher = (
  variables: SearchTagsQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<SearchTagsQuery, SearchTagsQueryVariables>(
    SearchTagsDocument,
    variables,
    options,
  );

export const AudioImageUpdatedDocument = `
    subscription audioImageUpdated($feedId: String!, $startTime: DateTime!, $endTime: DateTime!) {
  audioImageUpdated(feedId: $feedId, startTime: $startTime, endTime: $endTime) {
    created {
      ...AudioImageParts
    }
    updated {
      ...AudioImageParts
    }
  }
}
    ${AudioImagePartsFragmentDoc}`;
export const BoutNotificationSentDocument = `
    subscription boutNotificationSent($boutId: String!) {
  boutNotificationSent(boutId: $boutId) {
    updated {
      ...NotificationParts
    }
  }
}
    ${NotificationPartsFragmentDoc}`;
