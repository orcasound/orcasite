export type Maybe<T> = T | null;
export type InputMaybe<T> = Maybe<T>;
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: { input: string; output: string };
  String: { input: string; output: string };
  Boolean: { input: boolean; output: boolean };
  Int: { input: number; output: number };
  Float: { input: number; output: number };
  /**
   * The `DateTime` scalar type represents a date and time in the UTC
   * timezone. The DateTime appears in a JSON response as an ISO8601 formatted
   * string, including UTC timezone ("Z"). The parsed date and time string will
   * be converted to UTC if there is an offset.
   */
  DateTime: { input: Date; output: Date };
  /**
   * The `Decimal` scalar type represents signed double-precision fractional
   * values parsed by the `Decimal` library. The Decimal appears in a JSON
   * response as a string to preserve precision.
   */
  Decimal: { input: number; output: number };
  /**
   * The `Json` scalar type represents arbitrary json string data, represented as UTF-8
   * character sequences. The Json type is most often used to represent a free-form
   * human-readable json string.
   */
  Json: { input: { [key: string]: any }; output: { [key: string]: any } };
};

export const AudioCategory = {
  Anthrophony: "ANTHROPHONY",
  Biophony: "BIOPHONY",
  Geophony: "GEOPHONY",
} as const;

export type AudioCategory = (typeof AudioCategory)[keyof typeof AudioCategory];
/** Spectrograms or any other type of image representing audio. */
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

/** Spectrograms or any other type of image representing audio. */
export type AudioImageAudioImageFeedSegmentsArgs = {
  filter?: InputMaybe<AudioImageFeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageFeedSegmentSortInput>>>;
};

/** Spectrograms or any other type of image representing audio. */
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
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
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
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
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
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type AudioImageFilterBucketRegion = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type AudioImageFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
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
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type AudioImageFilterImageSize = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type AudioImageFilterImageType = {
  eq?: InputMaybe<ImageType>;
  greaterThan?: InputMaybe<ImageType>;
  greaterThanOrEqual?: InputMaybe<ImageType>;
  in?: InputMaybe<Array<InputMaybe<ImageType>>>;
  isDistinctFrom?: InputMaybe<ImageType>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<ImageType>;
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
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type AudioImageFilterParameters = {
  eq?: InputMaybe<Scalars["Json"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Json"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Json"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Json"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Json"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Json"]["input"]>;
  lessThan?: InputMaybe<Scalars["Json"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Json"]["input"]>;
  notEq?: InputMaybe<Scalars["Json"]["input"]>;
};

export type AudioImageFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type AudioImageFilterStatus = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
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

/** A moderator-generated time interval for a feed where there's a specific category of audio going on. Usually 10-90 minutes long. */
export type Bout = {
  __typename?: "Bout";
  category: AudioCategory;
  duration?: Maybe<Scalars["Decimal"]["output"]>;
  endTime?: Maybe<Scalars["DateTime"]["output"]>;
  /** JSON file for exporting the bout and its feed segments */
  exportJson?: Maybe<Scalars["String"]["output"]>;
  exportJsonFileName?: Maybe<Scalars["String"]["output"]>;
  exportScript?: Maybe<Scalars["String"]["output"]>;
  exportScriptFileName?: Maybe<Scalars["String"]["output"]>;
  feed?: Maybe<Feed>;
  feedId?: Maybe<Scalars["ID"]["output"]>;
  feedSegments: Array<FeedSegment>;
  feedStreams: Array<FeedStream>;
  id: Scalars["ID"]["output"];
  itemTags: Array<ItemTag>;
  name?: Maybe<Scalars["String"]["output"]>;
  startTime: Scalars["DateTime"]["output"];
  tags: Array<Tag>;
};

/** A moderator-generated time interval for a feed where there's a specific category of audio going on. Usually 10-90 minutes long. */
export type BoutFeedSegmentsArgs = {
  filter?: InputMaybe<FeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedSegmentSortInput>>>;
};

/** A moderator-generated time interval for a feed where there's a specific category of audio going on. Usually 10-90 minutes long. */
export type BoutFeedStreamsArgs = {
  filter?: InputMaybe<FeedStreamFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedStreamSortInput>>>;
};

/** A moderator-generated time interval for a feed where there's a specific category of audio going on. Usually 10-90 minutes long. */
export type BoutItemTagsArgs = {
  filter?: InputMaybe<ItemTagFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<ItemTagSortInput>>>;
};

/** A moderator-generated time interval for a feed where there's a specific category of audio going on. Usually 10-90 minutes long. */
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
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
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
  isDistinctFrom?: InputMaybe<AudioCategory>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<AudioCategory>;
  lessThan?: InputMaybe<AudioCategory>;
  lessThanOrEqual?: InputMaybe<AudioCategory>;
  notEq?: InputMaybe<AudioCategory>;
};

export type BoutFilterDuration = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Decimal"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type BoutFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
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
  feedSegments?: InputMaybe<FeedSegmentFilterInput>;
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
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type BoutFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
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

/** Groups one or many detections based on whether detections of the same category (whale, vessel, other) are within 3 minutes of each other */
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

/** Groups one or many detections based on whether detections of the same category (whale, vessel, other) are within 3 minutes of each other */
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
  isDistinctFrom?: InputMaybe<AudioCategory>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<AudioCategory>;
  lessThan?: InputMaybe<AudioCategory>;
  lessThanOrEqual?: InputMaybe<AudioCategory>;
  notEq?: InputMaybe<AudioCategory>;
};

export type CandidateFilterCategory = {
  eq?: InputMaybe<DetectionCategory>;
  greaterThan?: InputMaybe<DetectionCategory>;
  greaterThanOrEqual?: InputMaybe<DetectionCategory>;
  in?: InputMaybe<Array<InputMaybe<DetectionCategory>>>;
  isDistinctFrom?: InputMaybe<DetectionCategory>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<DetectionCategory>;
  lessThan?: InputMaybe<DetectionCategory>;
  lessThanOrEqual?: InputMaybe<DetectionCategory>;
  notEq?: InputMaybe<DetectionCategory>;
};

export type CandidateFilterDetectionCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
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
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type CandidateFilterMinTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type CandidateFilterVisible = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
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
  bout?: InputMaybe<ItemTagBoutTagBoutInput>;
  /**
   * How sure the moderator was that this tag belongs on this bout. On the application,
   * not the tag, because `L` is certain on one bout and a hedge on the next; a `?` in
   * the bout's name is where that hedge went before this column existed. Three words
   * rather than a number: a listening moderator has no probability, and a numeric field
   * invites a UI to invent one. Nil means nobody was asked, which is every application
   * made before the column existed, and is deliberately distinct from `certain`.
   */
  certainty?: InputMaybe<Scalars["String"]["input"]>;
  tag?: InputMaybe<ItemTagBoutTagTagInput>;
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
  /**
   * The identifier this tag cites in an external catalogue, as a CURIE or a full IRI.
   * An `animal` tag cites the salish-sea/animals register: `SSA:0000020` is J pod.
   * Unlike the name and the slug, it survives the tag being renamed. Nil is normal:
   * free-text tags stay legal, and an `animal` tag with no iri is how a gap in the
   * register shows up.
   */
  iri?: InputMaybe<Scalars["String"]["input"]>;
  /**
   * What the tag names: an `animal` (a species, ecotype, pod, matriline or individual),
   * a `signal` (a call type such as S01), or `other` (vessels, recording quality,
   * project markers). `other` is an answer, not a fallback -- it tells a consumer the
   * tag is safe to skip. Nil means nobody has classified the tag yet.
   */
  kind?: InputMaybe<Scalars["String"]["input"]>;
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

/** A single user-submitted report of tagged audio (whale, vessel, other) */
export type Detection = {
  __typename?: "Detection";
  candidate?: Maybe<Candidate>;
  candidateId?: Maybe<Scalars["ID"]["output"]>;
  category?: Maybe<DetectionCategory>;
  description?: Maybe<Scalars["String"]["output"]>;
  feed: Feed;
  feedId: Scalars["ID"]["output"];
  id: Scalars["ID"]["output"];
  /** Optional unique key for this detection */
  idempotencyKey?: Maybe<Scalars["String"]["output"]>;
  listenerCount?: Maybe<Scalars["Int"]["output"]>;
  playerOffset: Scalars["Decimal"]["output"];
  playlistTimestamp: Scalars["Int"]["output"];
  source: DetectionSource;
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
  isDistinctFrom?: InputMaybe<DetectionCategory>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<DetectionCategory>;
  lessThan?: InputMaybe<DetectionCategory>;
  lessThanOrEqual?: InputMaybe<DetectionCategory>;
  notEq?: InputMaybe<DetectionCategory>;
};

export type DetectionFilterDescription = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type DetectionFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type DetectionFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type DetectionFilterIdempotencyKey = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
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
  /** Optional unique key for this detection */
  idempotencyKey?: InputMaybe<DetectionFilterIdempotencyKey>;
  listenerCount?: InputMaybe<DetectionFilterListenerCount>;
  not?: InputMaybe<Array<DetectionFilterInput>>;
  or?: InputMaybe<Array<DetectionFilterInput>>;
  playerOffset?: InputMaybe<DetectionFilterPlayerOffset>;
  playlistTimestamp?: InputMaybe<DetectionFilterPlaylistTimestamp>;
  source?: InputMaybe<DetectionFilterSource>;
  sourceIp?: InputMaybe<DetectionFilterSourceIp>;
  timestamp?: InputMaybe<DetectionFilterTimestamp>;
  visible?: InputMaybe<DetectionFilterVisible>;
};

export type DetectionFilterListenerCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type DetectionFilterPlayerOffset = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<Scalars["Decimal"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type DetectionFilterPlaylistTimestamp = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<Scalars["Int"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type DetectionFilterSource = {
  eq?: InputMaybe<DetectionSource>;
  greaterThan?: InputMaybe<DetectionSource>;
  greaterThanOrEqual?: InputMaybe<DetectionSource>;
  in?: InputMaybe<Array<DetectionSource>>;
  isDistinctFrom?: InputMaybe<DetectionSource>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<DetectionSource>;
  lessThan?: InputMaybe<DetectionSource>;
  lessThanOrEqual?: InputMaybe<DetectionSource>;
  notEq?: InputMaybe<DetectionSource>;
};

export type DetectionFilterSourceIp = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type DetectionFilterTimestamp = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type DetectionFilterVisible = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
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
  IdempotencyKey: "IDEMPOTENCY_KEY",
  ListenerCount: "LISTENER_COUNT",
  PlayerOffset: "PLAYER_OFFSET",
  PlaylistTimestamp: "PLAYLIST_TIMESTAMP",
  Source: "SOURCE",
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

export const DetectionSource = {
  Human: "HUMAN",
  Machine: "MACHINE",
} as const;

export type DetectionSource =
  (typeof DetectionSource)[keyof typeof DetectionSource];
/** Represents hydrophones that record audio under water */
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
  latestListenerCount?: Maybe<ListenerCount>;
  listenerCounts: Array<ListenerCount>;
  locationPoint: Scalars["Json"]["output"];
  maintainerEmails?: Maybe<Array<Scalars["String"]["output"]>>;
  mapUrl?: Maybe<Scalars["String"]["output"]>;
  name: Scalars["String"]["output"];
  nodeName: Scalars["String"]["output"];
  online?: Maybe<Scalars["Boolean"]["output"]>;
  orcahelloId?: Maybe<Scalars["String"]["output"]>;
  slug: Scalars["String"]["output"];
  thumbUrl?: Maybe<Scalars["String"]["output"]>;
  visible?: Maybe<Scalars["Boolean"]["output"]>;
};

/** Represents hydrophones that record audio under water */
export type FeedAudioImagesArgs = {
  filter?: InputMaybe<AudioImageFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageSortInput>>>;
};

/** Represents hydrophones that record audio under water */
export type FeedBoutsArgs = {
  filter?: InputMaybe<BoutFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<BoutSortInput>>>;
};

/** Represents hydrophones that record audio under water */
export type FeedFeedSegmentsArgs = {
  filter?: InputMaybe<FeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedSegmentSortInput>>>;
};

/** Represents hydrophones that record audio under water */
export type FeedFeedStreamsArgs = {
  filter?: InputMaybe<FeedStreamFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedStreamSortInput>>>;
};

/** Represents hydrophones that record audio under water */
export type FeedListenerCountsArgs = {
  filter?: InputMaybe<ListenerCountFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<ListenerCountSortInput>>>;
};

export type FeedFilterBucket = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterBucketRegion = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterCloudfrontUrl = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterDataplicityId = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterImageUrl = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
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
  latestListenerCount?: InputMaybe<ListenerCountFilterInput>;
  listenerCounts?: InputMaybe<ListenerCountFilterInput>;
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
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterLocationPoint = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterName = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterNodeName = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterOnline = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedFilterOrcahelloId = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterSlug = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedFilterVisible = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Boolean"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

/** Represents a single .ts file from a feed (usually 10 second). A feed_stream has many feed_segments. Used to track timestamps for querying audio from S3 */
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
  /** Start time declared by the node in the playlist's #EXT-X-PROGRAM-DATE-TIME tag, when present. Recorded but not yet used for start_time; see orcasite#1041 */
  programDateTime?: Maybe<Scalars["DateTime"]["output"]>;
  /** S3 object path for ts file (e.g. /rpi_orcasound_lab/hls/1541027406/live005.ts) */
  segmentPath?: Maybe<Scalars["String"]["output"]>;
  startTime?: Maybe<Scalars["DateTime"]["output"]>;
};

/** Represents a single .ts file from a feed (usually 10 second). A feed_stream has many feed_segments. Used to track timestamps for querying audio from S3 */
export type FeedSegmentAudioImageFeedSegmentsArgs = {
  filter?: InputMaybe<AudioImageFeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageFeedSegmentSortInput>>>;
};

/** Represents a single .ts file from a feed (usually 10 second). A feed_stream has many feed_segments. Used to track timestamps for querying audio from S3 */
export type FeedSegmentAudioImagesArgs = {
  filter?: InputMaybe<AudioImageFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<AudioImageSortInput>>>;
};

export type FeedSegmentFilterBucket = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterBucketRegion = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterCloudfrontUrl = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterDuration = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Decimal"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type FeedSegmentFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
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
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
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
  /** Start time declared by the node in the playlist's #EXT-X-PROGRAM-DATE-TIME tag, when present. Recorded but not yet used for start_time; see orcasite#1041 */
  programDateTime?: InputMaybe<FeedSegmentFilterProgramDateTime>;
  /** S3 object path for ts file (e.g. /rpi_orcasound_lab/hls/1541027406/live005.ts) */
  segmentPath?: InputMaybe<FeedSegmentFilterSegmentPath>;
  startTime?: InputMaybe<FeedSegmentFilterStartTime>;
};

export type FeedSegmentFilterPlaylistM3u8Path = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterPlaylistPath = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterPlaylistTimestamp = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterProgramDateTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type FeedSegmentFilterSegmentPath = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedSegmentFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
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
  ProgramDateTime: "PROGRAM_DATE_TIME",
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

/**
 * Represents an m3u8 file in S3 (audio segment manifest, lists .ts files in order with their length).
 *   Whenever a feed restarts, a new m3u8 file is created
 */
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

/**
 * Represents an m3u8 file in S3 (audio segment manifest, lists .ts files in order with their length).
 *   Whenever a feed restarts, a new m3u8 file is created
 */
export type FeedStreamBoutFeedStreamsArgs = {
  filter?: InputMaybe<BoutFeedStreamFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<BoutFeedStreamSortInput>>>;
};

/**
 * Represents an m3u8 file in S3 (audio segment manifest, lists .ts files in order with their length).
 *   Whenever a feed restarts, a new m3u8 file is created
 */
export type FeedStreamBoutsArgs = {
  filter?: InputMaybe<BoutFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<BoutSortInput>>>;
};

/**
 * Represents an m3u8 file in S3 (audio segment manifest, lists .ts files in order with their length).
 *   Whenever a feed restarts, a new m3u8 file is created
 */
export type FeedStreamFeedSegmentsArgs = {
  filter?: InputMaybe<FeedSegmentFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<FeedSegmentSortInput>>>;
};

export type FeedStreamFilterBucket = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterBucketRegion = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterCloudfrontUrl = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterDuration = {
  eq?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Decimal"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThan?: InputMaybe<Scalars["Decimal"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Decimal"]["input"]>;
  notEq?: InputMaybe<Scalars["Decimal"]["input"]>;
};

export type FeedStreamFilterEndTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
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
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterPlaylistPath = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterPlaylistTimestamp = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type FeedStreamFilterPrevFeedStreamId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type FeedStreamFilterStartTime = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
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
/** Tag applied by a user to an item (currently just bouts), and how sure they were */
export type ItemTag = {
  __typename?: "ItemTag";
  bout?: Maybe<Bout>;
  boutId?: Maybe<Scalars["ID"]["output"]>;
  /**
   * How sure the moderator was that this tag belongs on this bout. On the application,
   * not the tag, because `L` is certain on one bout and a hedge on the next; a `?` in
   * the bout's name is where that hedge went before this column existed. Three words
   * rather than a number: a listening moderator has no probability, and a numeric field
   * invites a UI to invent one. Nil means nobody was asked, which is every application
   * made before the column existed, and is deliberately distinct from `certain`.
   */
  certainty?: Maybe<Scalars["String"]["output"]>;
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
  /**
   * The identifier this tag cites in an external catalogue, as a CURIE or a full IRI.
   * An `animal` tag cites the salish-sea/animals register: `SSA:0000020` is J pod.
   * Unlike the name and the slug, it survives the tag being renamed. Nil is normal:
   * free-text tags stay legal, and an `animal` tag with no iri is how a gap in the
   * register shows up.
   */
  iri?: InputMaybe<Scalars["String"]["input"]>;
  /**
   * What the tag names: an `animal` (a species, ecotype, pod, matriline or individual),
   * a `signal` (a call type such as S01), or `other` (vessels, recording quality,
   * project markers). `other` is an answer, not a fallback -- it tells a consumer the
   * tag is safe to skip. Nil means nobody has classified the tag yet.
   */
  kind?: InputMaybe<Scalars["String"]["input"]>;
  name?: InputMaybe<Scalars["String"]["input"]>;
};

export type ItemTagFilterBoutId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type ItemTagFilterCertainty = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type ItemTagFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type ItemTagFilterInput = {
  and?: InputMaybe<Array<ItemTagFilterInput>>;
  bout?: InputMaybe<BoutFilterInput>;
  boutId?: InputMaybe<ItemTagFilterBoutId>;
  /**
   * How sure the moderator was that this tag belongs on this bout. On the application,
   * not the tag, because `L` is certain on one bout and a hedge on the next; a `?` in
   * the bout's name is where that hedge went before this column existed. Three words
   * rather than a number: a listening moderator has no probability, and a numeric field
   * invites a UI to invent one. Nil means nobody was asked, which is every application
   * made before the column existed, and is deliberately distinct from `certain`.
   */
  certainty?: InputMaybe<ItemTagFilterCertainty>;
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
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type ItemTagFilterUserId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["ID"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export const ItemTagSortField = {
  BoutId: "BOUT_ID",
  Certainty: "CERTAINTY",
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

/** Track listener counts for each feed */
export type ListenerCount = {
  __typename?: "ListenerCount";
  count: Scalars["Int"]["output"];
  feed?: Maybe<Feed>;
  feedId?: Maybe<Scalars["ID"]["output"]>;
  id: Scalars["ID"]["output"];
  timestamp: Scalars["DateTime"]["output"];
};

export type ListenerCountFilterCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<Scalars["Int"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type ListenerCountFilterFeedId = {
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type ListenerCountFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type ListenerCountFilterInput = {
  and?: InputMaybe<Array<ListenerCountFilterInput>>;
  count?: InputMaybe<ListenerCountFilterCount>;
  feed?: InputMaybe<FeedFilterInput>;
  feedId?: InputMaybe<ListenerCountFilterFeedId>;
  id?: InputMaybe<ListenerCountFilterId>;
  not?: InputMaybe<Array<ListenerCountFilterInput>>;
  or?: InputMaybe<Array<ListenerCountFilterInput>>;
  timestamp?: InputMaybe<ListenerCountFilterTimestamp>;
};

export type ListenerCountFilterTimestamp = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<Scalars["DateTime"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export const ListenerCountSortField = {
  Count: "COUNT",
  FeedId: "FEED_ID",
  Id: "ID",
  Timestamp: "TIMESTAMP",
} as const;

export type ListenerCountSortField =
  (typeof ListenerCountSortField)[keyof typeof ListenerCountSortField];
export type ListenerCountSortInput = {
  field: ListenerCountSortField;
  order?: InputMaybe<SortOrder>;
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
  /** The path to the field that produced the error */
  path?: Maybe<Array<Scalars["String"]["output"]>>;
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
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type NotificationFilterEventType = {
  eq?: InputMaybe<NotificationEventType>;
  greaterThan?: InputMaybe<NotificationEventType>;
  greaterThanOrEqual?: InputMaybe<NotificationEventType>;
  in?: InputMaybe<Array<InputMaybe<NotificationEventType>>>;
  isDistinctFrom?: InputMaybe<NotificationEventType>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<NotificationEventType>;
  lessThan?: InputMaybe<NotificationEventType>;
  lessThanOrEqual?: InputMaybe<NotificationEventType>;
  notEq?: InputMaybe<NotificationEventType>;
};

export type NotificationFilterFinished = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<Scalars["Boolean"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type NotificationFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
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
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type NotificationFilterNotifiedCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  lessThan?: InputMaybe<Scalars["Int"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  notEq?: InputMaybe<Scalars["Int"]["input"]>;
};

export type NotificationFilterNotifiedCountUpdatedAt = {
  eq?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["DateTime"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThan?: InputMaybe<Scalars["DateTime"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["DateTime"]["input"]>;
  notEq?: InputMaybe<Scalars["DateTime"]["input"]>;
};

export type NotificationFilterProgress = {
  eq?: InputMaybe<Scalars["Float"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Float"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Float"]["input"]>;
  in?: InputMaybe<Array<Scalars["Float"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["Float"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Float"]["input"]>;
  lessThan?: InputMaybe<Scalars["Float"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Float"]["input"]>;
  notEq?: InputMaybe<Scalars["Float"]["input"]>;
};

export type NotificationFilterTargetCount = {
  eq?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Int"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Int"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["Int"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Int"]["input"]>;
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
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
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
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
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
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
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
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
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
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
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
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
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
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<ItemTag>>;
};

/** A page of :listener_count */
export type PageOfListenerCount = {
  __typename?: "PageOfListenerCount";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
  /** The records contained in the page */
  results?: Maybe<Array<ListenerCount>>;
};

/** A page of :tag */
export type PageOfTag = {
  __typename?: "PageOfTag";
  /** Total count on all pages */
  count?: Maybe<Scalars["Int"]["output"]>;
  /** Whether or not there is a next page */
  hasNextPage: Scalars["Boolean"]["output"];
  /** Whether or not there is a previous page */
  hasPreviousPage: Scalars["Boolean"]["output"];
  /** The number of the last page */
  lastPage: Scalars["Int"]["output"];
  /** The number of records per page */
  limit: Scalars["Int"]["output"];
  /** The number of the current page */
  pageNumber: Scalars["Int"]["output"];
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
  /** Seed feeds, then the rest of the resources */
  seedAll: Array<Seed>;
  seedFeeds: SeedFeedsResult;
  seedLatestResource: SeedLatestResourceResult;
  seedResource: SeedResourceResult;
  setBoutTagCertainty: SetBoutTagCertaintyResult;
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
  input?: InputMaybe<CreateBoutTagInput>;
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

export type RootMutationTypeSeedAllArgs = {
  input?: InputMaybe<SeedAllInput>;
};

export type RootMutationTypeSeedLatestResourceArgs = {
  input: SeedLatestResourceInput;
};

export type RootMutationTypeSeedResourceArgs = {
  input: SeedResourceInput;
};

export type RootMutationTypeSetBoutTagCertaintyArgs = {
  id: Scalars["ID"]["input"];
  input?: InputMaybe<SetBoutTagCertaintyInput>;
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
  listenerCount?: Maybe<PageOfListenerCount>;
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
  slug: Scalars["String"]["input"];
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

export type RootQueryTypeListenerCountArgs = {
  feedId: Scalars["String"]["input"];
  filter?: InputMaybe<ListenerCountFilterInput>;
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  offset?: InputMaybe<Scalars["Int"]["input"]>;
  sort?: InputMaybe<Array<InputMaybe<ListenerCountSortInput>>>;
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

/** Non-persisted resource to seed records from specific time ranges from Orcasite prod */
export type Seed = {
  __typename?: "Seed";
  endTime?: Maybe<Scalars["DateTime"]["output"]>;
  feedId?: Maybe<Scalars["String"]["output"]>;
  id: Scalars["ID"]["output"];
  limit?: Maybe<Scalars["Int"]["output"]>;
  resource: SeedResource;
  seededCount?: Maybe<Scalars["Int"]["output"]>;
  startTime?: Maybe<Scalars["DateTime"]["output"]>;
};

export type SeedAllInput = {
  endTime?: InputMaybe<Scalars["DateTime"]["input"]>;
  startTime?: InputMaybe<Scalars["DateTime"]["input"]>;
};

/** The result of the :seed_feeds mutation */
export type SeedFeedsResult = {
  __typename?: "SeedFeedsResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Seed>;
};

export type SeedLatestResourceInput = {
  /** Local/dev server feed ID to seed relationship */
  feedId: Scalars["String"]["input"];
  limit?: InputMaybe<Scalars["Int"]["input"]>;
  resource: SeedResource;
};

/** The result of the :seed_latest_resource mutation */
export type SeedLatestResourceResult = {
  __typename?: "SeedLatestResourceResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Seed>;
};

export const SeedResource = {
  AudioImage: "AUDIO_IMAGE",
  Bout: "BOUT",
  Candidate: "CANDIDATE",
  Detection: "DETECTION",
  Feed: "FEED",
  FeedSegment: "FEED_SEGMENT",
  FeedStream: "FEED_STREAM",
} as const;

export type SeedResource = (typeof SeedResource)[keyof typeof SeedResource];
export type SeedResourceInput = {
  endTime?: InputMaybe<Scalars["DateTime"]["input"]>;
  /** Local/dev server feed ID to seed relationship */
  feedId: Scalars["String"]["input"];
  resource: SeedResource;
  startTime?: InputMaybe<Scalars["DateTime"]["input"]>;
};

/** The result of the :seed_resource mutation */
export type SeedResourceResult = {
  __typename?: "SeedResourceResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<Seed>;
};

export type SetBoutTagCertaintyInput = {
  /**
   * How sure the moderator was that this tag belongs on this bout. On the application,
   * not the tag, because `L` is certain on one bout and a hedge on the next; a `?` in
   * the bout's name is where that hedge went before this column existed. Three words
   * rather than a number: a listening moderator has no probability, and a numeric field
   * invites a UI to invent one. Nil means nobody was asked, which is every application
   * made before the column existed, and is deliberately distinct from `certain`.
   */
  certainty?: InputMaybe<Scalars["String"]["input"]>;
};

/** The result of the :set_bout_tag_certainty mutation */
export type SetBoutTagCertaintyResult = {
  __typename?: "SetBoutTagCertaintyResult";
  /** Any errors generated, if the mutation failed */
  errors: Array<MutationError>;
  /** The successful result of the mutation */
  result?: Maybe<ItemTag>;
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
  category?: InputMaybe<DetectionCategory>;
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

/** Tag definition with a name, description, unique slug, and optionally what kind of thing it names and the external identifier for it */
export type Tag = {
  __typename?: "Tag";
  description?: Maybe<Scalars["String"]["output"]>;
  id: Scalars["ID"]["output"];
  /**
   * The identifier this tag cites in an external catalogue, as a CURIE or a full IRI.
   * An `animal` tag cites the salish-sea/animals register: `SSA:0000020` is J pod.
   * Unlike the name and the slug, it survives the tag being renamed. Nil is normal:
   * free-text tags stay legal, and an `animal` tag with no iri is how a gap in the
   * register shows up.
   */
  iri?: Maybe<Scalars["String"]["output"]>;
  /**
   * What the tag names: an `animal` (a species, ecotype, pod, matriline or individual),
   * a `signal` (a call type such as S01), or `other` (vessels, recording quality,
   * project markers). `other` is an answer, not a fallback -- it tells a consumer the
   * tag is safe to skip. Nil means nobody has classified the tag yet.
   */
  kind?: Maybe<Scalars["String"]["output"]>;
  name: Scalars["String"]["output"];
  slug: Scalars["String"]["output"];
};

export type TagFilterDescription = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type TagFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type TagFilterInput = {
  and?: InputMaybe<Array<TagFilterInput>>;
  description?: InputMaybe<TagFilterDescription>;
  id?: InputMaybe<TagFilterId>;
  /**
   * The identifier this tag cites in an external catalogue, as a CURIE or a full IRI.
   * An `animal` tag cites the salish-sea/animals register: `SSA:0000020` is J pod.
   * Unlike the name and the slug, it survives the tag being renamed. Nil is normal:
   * free-text tags stay legal, and an `animal` tag with no iri is how a gap in the
   * register shows up.
   */
  iri?: InputMaybe<TagFilterIri>;
  /**
   * What the tag names: an `animal` (a species, ecotype, pod, matriline or individual),
   * a `signal` (a call type such as S01), or `other` (vessels, recording quality,
   * project markers). `other` is an answer, not a fallback -- it tells a consumer the
   * tag is safe to skip. Nil means nobody has classified the tag yet.
   */
  kind?: InputMaybe<TagFilterKind>;
  name?: InputMaybe<TagFilterName>;
  not?: InputMaybe<Array<TagFilterInput>>;
  or?: InputMaybe<Array<TagFilterInput>>;
  slug?: InputMaybe<TagFilterSlug>;
};

export type TagFilterIri = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type TagFilterKind = {
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
};

export type TagFilterName = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type TagFilterSlug = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export const TagSortField = {
  Description: "DESCRIPTION",
  Id: "ID",
  Iri: "IRI",
  Kind: "KIND",
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
  detectionBot: Scalars["Boolean"]["output"];
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
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type UserFilterDetectionBot = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<Scalars["Boolean"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type UserFilterEmail = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<Scalars["String"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserFilterFirstName = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserFilterId = {
  eq?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThan?: InputMaybe<Scalars["ID"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  in?: InputMaybe<Array<Scalars["ID"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["ID"]["input"]>;
  lessThan?: InputMaybe<Scalars["ID"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["ID"]["input"]>;
  notEq?: InputMaybe<Scalars["ID"]["input"]>;
};

export type UserFilterInput = {
  admin?: InputMaybe<UserFilterAdmin>;
  and?: InputMaybe<Array<UserFilterInput>>;
  detectionBot?: InputMaybe<UserFilterDetectionBot>;
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
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserFilterModerator = {
  eq?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  in?: InputMaybe<Array<Scalars["Boolean"]["input"]>>;
  isDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThan?: InputMaybe<Scalars["Boolean"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["Boolean"]["input"]>;
  notEq?: InputMaybe<Scalars["Boolean"]["input"]>;
};

export type UserFilterUsername = {
  contains?: InputMaybe<Scalars["String"]["input"]>;
  eq?: InputMaybe<Scalars["String"]["input"]>;
  greaterThan?: InputMaybe<Scalars["String"]["input"]>;
  greaterThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  ilike?: InputMaybe<Scalars["String"]["input"]>;
  in?: InputMaybe<Array<InputMaybe<Scalars["String"]["input"]>>>;
  isDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  isNil?: InputMaybe<Scalars["Boolean"]["input"]>;
  isNotDistinctFrom?: InputMaybe<Scalars["String"]["input"]>;
  lessThan?: InputMaybe<Scalars["String"]["input"]>;
  lessThanOrEqual?: InputMaybe<Scalars["String"]["input"]>;
  like?: InputMaybe<Scalars["String"]["input"]>;
  notEq?: InputMaybe<Scalars["String"]["input"]>;
  stringEndsWith?: InputMaybe<Scalars["String"]["input"]>;
  stringStartsWith?: InputMaybe<Scalars["String"]["input"]>;
};

export type UserWithToken = {
  __typename?: "UserWithToken";
  admin?: Maybe<Scalars["Boolean"]["output"]>;
  detectionBot: Scalars["Boolean"]["output"];
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
