/** Internal type. DO NOT USE DIRECTLY. */
type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
/** Internal type. DO NOT USE DIRECTLY. */
export type Incremental<T> =
  | T
  | {
      [P in keyof T]?: P extends " $fragmentName" | "__typename" ? T[P] : never;
    };
import type * as Types from "./schema";

import { DocumentTypeDecoration } from "@graphql-typed-document-node/core";
import {
  useMutation,
  useQuery,
  UseMutationOptions,
  UseQueryOptions,
} from "@tanstack/react-query";
import { fetcher } from "@/graphql/client";
export * from "./schema";
export type AudioImagePartsFragment = {
  id: string;
  startTime: Date;
  endTime: Date;
  status: string;
  objectPath: string | null;
  bucket: string | null;
  bucketRegion: string | null;
  feedId: string;
  imageSize: number | null;
  imageType: Types.ImageType | null;
};

export type BoutPartsFragment = {
  id: string;
  name: string | null;
  category: Types.AudioCategory;
  duration: number | null;
  endTime: Date | null;
  startTime: Date;
};

export type CandidatePartsFragment = {
  id: string;
  minTime: Date;
  maxTime: Date;
  category: Types.DetectionCategory | null;
  detectionCount: number | null;
  visible: boolean | null;
};

export type DetectionPartsFragment = {
  id: string;
  category: Types.DetectionCategory | null;
  description: string | null;
  listenerCount: number | null;
  playlistTimestamp: number;
  playerOffset: number;
  timestamp: Date;
  visible: boolean | null;
  sourceIp: string | null;
  source: Types.DetectionSource;
  feedId: string;
};

export type ErrorPartsFragment = {
  code: string | null;
  fields: Array<string> | null;
  message: string | null;
  shortMessage: string | null;
  vars: { [key: string]: any } | null;
};

export type FeedPartsFragment = {
  id: string;
  name: string;
  slug: string;
  nodeName: string;
  introHtml: string | null;
  thumbUrl: string | null;
  imageUrl: string | null;
  mapUrl: string | null;
  bucket: string;
  latLng: { lat: number; lng: number };
};

export type FeedSegmentPartsFragment = {
  id: string;
  startTime: Date | null;
  endTime: Date | null;
  duration: number | null;
  bucket: string | null;
  bucketRegion: string | null;
  cloudfrontUrl: string | null;
  fileName: string;
  playlistM3u8Path: string | null;
  playlistPath: string | null;
  playlistTimestamp: string | null;
  segmentPath: string | null;
};

export type FeedStreamPartsFragment = {
  id: string;
  startTime: Date | null;
  endTime: Date | null;
  duration: number | null;
  bucket: string | null;
  bucketRegion: string | null;
  cloudfrontUrl: string | null;
  playlistTimestamp: string | null;
  playlistPath: string | null;
  playlistM3u8Path: string | null;
};

export type ItemTagPartsFragment = {
  id: string;
  user: { username: string | null } | null;
  tag: {
    id: string;
    name: string;
    slug: string;
    description: string | null;
  } | null;
};

export type NotificationPartsFragment = {
  id: string;
  active: boolean | null;
  eventType: Types.NotificationEventType | null;
  progress: number | null;
  targetCount: number | null;
  finished: boolean | null;
  notifiedCount: number | null;
  notifiedCountUpdatedAt: Date | null;
  insertedAt: Date;
};

export type SeedPartsFragment = {
  id: string;
  resource: Types.SeedResource;
  startTime: Date | null;
  endTime: Date | null;
  seededCount: number | null;
};

export type TagPartsFragment = {
  id: string;
  name: string;
  description: string | null;
  slug: string;
};

export type CancelCandidateNotificationsMutationVariables = Exact<{
  candidateId: string | number;
}>;

export type CancelCandidateNotificationsMutation = {
  cancelCandidateNotifications: {
    result: { id: string } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type CancelNotificationMutationVariables = Exact<{
  id: string | number;
}>;

export type CancelNotificationMutation = {
  cancelNotification: {
    result: {
      id: string;
      active: boolean | null;
      insertedAt: Date;
      targetCount: number | null;
      notifiedCount: number | null;
      notifiedCountUpdatedAt: Date | null;
      progress: number | null;
      finished: boolean | null;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type CreateBoutMutationVariables = Exact<{
  feedId: string;
  name?: string | null | undefined;
  startTime: Date;
  endTime?: Date | null | undefined;
  category: Types.AudioCategory;
}>;

export type CreateBoutMutation = {
  createBout: {
    result: {
      id: string;
      name: string | null;
      category: Types.AudioCategory;
      duration: number | null;
      endTime: Date | null;
      startTime: Date;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type CreateBoutTagMutationVariables = Exact<{
  tagId?: string | number | null | undefined;
  tagName: string;
  tagDescription?: string | null | undefined;
  boutId: string | number;
}>;

export type CreateBoutTagMutation = {
  createBoutTag: {
    result: {
      id: string;
      user: { username: string | null } | null;
      tag: {
        id: string;
        name: string;
        slug: string;
        description: string | null;
      } | null;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type DeleteBoutTagMutationVariables = Exact<{
  boutTagId: string | number;
}>;

export type DeleteBoutTagMutation = {
  deleteBoutTag: {
    result: {
      id: string;
      user: { username: string | null } | null;
      tag: {
        id: string;
        name: string;
        slug: string;
        description: string | null;
      } | null;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type GenerateFeedSpectrogramsMutationVariables = Exact<{
  feedId: string | number;
  startTime: Date;
  endTime: Date;
}>;

export type GenerateFeedSpectrogramsMutation = {
  generateFeedSpectrograms: {
    result: {
      id: string;
      name: string;
      slug: string;
      nodeName: string;
      introHtml: string | null;
      thumbUrl: string | null;
      imageUrl: string | null;
      mapUrl: string | null;
      bucket: string;
      latLng: { lat: number; lng: number };
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type NotifyConfirmedCandidateMutationVariables = Exact<{
  candidateId: string;
  message: string;
}>;

export type NotifyConfirmedCandidateMutation = {
  notifyConfirmedCandidate: {
    result: {
      id: string;
      eventType: Types.NotificationEventType | null;
      active: boolean | null;
      targetCount: number | null;
      notifiedCount: number | null;
      progress: number | null;
      finished: boolean | null;
      notifiedCountUpdatedAt: Date | null;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type NotifyLiveBoutMutationVariables = Exact<{
  boutId: string;
  message: string;
}>;

export type NotifyLiveBoutMutation = {
  notifyLiveBout: {
    result: {
      id: string;
      eventType: Types.NotificationEventType | null;
      active: boolean | null;
      targetCount: number | null;
      notifiedCount: number | null;
      progress: number | null;
      finished: boolean | null;
      notifiedCountUpdatedAt: Date | null;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type RegisterWithPasswordMutationVariables = Exact<{
  firstName?: string | null | undefined;
  lastName?: string | null | undefined;
  email: string;
  username: string;
  password: string;
  passwordConfirmation: string;
}>;

export type RegisterWithPasswordMutation = {
  registerWithPassword: {
    result: {
      id: string;
      email: string | null;
      username: string | null;
      admin: boolean | null;
      firstName: string | null;
      lastName: string | null;
    } | null;
    errors: Array<{
      message: string | null;
      code: string | null;
      fields: Array<string> | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type RequestPasswordResetMutationVariables = Exact<{
  email: string;
}>;

export type RequestPasswordResetMutation = {
  requestPasswordReset: boolean | null;
};

export type ResetPasswordMutationVariables = Exact<{
  password: string;
  passwordConfirmation: string;
  resetToken: string;
}>;

export type ResetPasswordMutation = {
  resetPassword: {
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    } | null> | null;
    user: {
      id: string;
      email: string | null;
      firstName: string | null;
      lastName: string | null;
      admin: boolean | null;
    } | null;
  } | null;
};

export type SeedAllMutationVariables = Exact<{
  startTime: Date;
  endTime: Date;
}>;

export type SeedAllMutation = {
  seedAll: Array<{
    id: string;
    resource: Types.SeedResource;
    startTime: Date | null;
    endTime: Date | null;
    seededCount: number | null;
  }>;
};

export type SeedFeedsMutationVariables = Exact<{ [key: string]: never }>;

export type SeedFeedsMutation = {
  seedFeeds: {
    result: {
      id: string;
      resource: Types.SeedResource;
      startTime: Date | null;
      endTime: Date | null;
      seededCount: number | null;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type SeedResourceMutationVariables = Exact<{
  resource: Types.SeedResource;
  feedId: string;
  startTime: Date;
  endTime: Date;
}>;

export type SeedResourceMutation = {
  seedResource: {
    result: {
      id: string;
      resource: Types.SeedResource;
      startTime: Date | null;
      endTime: Date | null;
      seededCount: number | null;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type SetDetectionVisibleMutationVariables = Exact<{
  id: string | number;
  visible: boolean;
}>;

export type SetDetectionVisibleMutation = {
  setDetectionVisible: {
    result: { id: string; visible: boolean | null } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type SignInWithPasswordMutationVariables = Exact<{
  email: string;
  password: string;
}>;

export type SignInWithPasswordMutation = {
  signInWithPassword: {
    user: {
      id: string;
      email: string | null;
      admin: boolean | null;
      firstName: string | null;
      lastName: string | null;
    } | null;
    errors: Array<{
      message: string | null;
      code: string | null;
      fields: Array<string> | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    } | null> | null;
  } | null;
};

export type SignOutMutationVariables = Exact<{ [key: string]: never }>;

export type SignOutMutation = { signOut: boolean | null };

export type SubmitDetectionMutationVariables = Exact<{
  feedId: string;
  playlistTimestamp: number;
  playerOffset: number;
  description: string;
  listenerCount?: number | null | undefined;
  category: Types.DetectionCategory;
}>;

export type SubmitDetectionMutation = {
  submitDetection: {
    result: { id: string } | null;
    errors: Array<{
      message: string | null;
      code: string | null;
      fields: Array<string> | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type UpdateBoutMutationVariables = Exact<{
  id: string | number;
  startTime: Date;
  endTime?: Date | null | undefined;
  name?: string | null | undefined;
  category: Types.AudioCategory;
}>;

export type UpdateBoutMutation = {
  updateBout: {
    result: {
      id: string;
      name: string | null;
      category: Types.AudioCategory;
      duration: number | null;
      endTime: Date | null;
      startTime: Date;
    } | null;
    errors: Array<{
      code: string | null;
      fields: Array<string> | null;
      message: string | null;
      shortMessage: string | null;
      vars: { [key: string]: any } | null;
    }>;
  };
};

export type BoutQueryVariables = Exact<{
  id: string | number;
}>;

export type BoutQuery = {
  bout: {
    id: string;
    name: string | null;
    category: Types.AudioCategory;
    duration: number | null;
    endTime: Date | null;
    startTime: Date;
    feed: {
      id: string;
      name: string;
      slug: string;
      nodeName: string;
      introHtml: string | null;
      thumbUrl: string | null;
      imageUrl: string | null;
      mapUrl: string | null;
      bucket: string;
      latLng: { lat: number; lng: number };
    } | null;
  } | null;
};

export type BoutExportQueryVariables = Exact<{
  boutId: string | number;
}>;

export type BoutExportQuery = {
  bout: {
    id: string;
    exportJson: string | null;
    exportJsonFileName: string | null;
    exportScript: string | null;
    exportScriptFileName: string | null;
  } | null;
};

export type CandidateQueryVariables = Exact<{
  id: string | number;
}>;

export type CandidateQuery = {
  candidate: {
    id: string;
    minTime: Date;
    maxTime: Date;
    category: Types.DetectionCategory | null;
    detectionCount: number | null;
    visible: boolean | null;
    feed: {
      id: string;
      slug: string;
      name: string;
      nodeName: string;
      bucket: string;
    };
    detections: Array<{
      id: string;
      category: Types.DetectionCategory | null;
      description: string | null;
      listenerCount: number | null;
      playlistTimestamp: number;
      playerOffset: number;
      timestamp: Date;
      visible: boolean | null;
      sourceIp: string | null;
      source: Types.DetectionSource;
      feedId: string;
    }>;
  } | null;
};

export type GetCurrentUserQueryVariables = Exact<{ [key: string]: never }>;

export type GetCurrentUserQuery = {
  currentUser: {
    id: string;
    firstName: string | null;
    lastName: string | null;
    username: string | null;
    email: string | null;
    admin: boolean | null;
    moderator: boolean | null;
    token: string | null;
  } | null;
};

export type DetectionsCountQueryVariables = Exact<{
  feedId: string;
  fromTime: Date;
  toTime?: Date | null | undefined;
  category?: Types.DetectionCategory | null | undefined;
}>;

export type DetectionsCountQuery = { feedDetectionsCount: number };

export type FeedQueryVariables = Exact<{
  slug: string;
}>;

export type FeedQuery = {
  feed: {
    id: string;
    name: string;
    slug: string;
    nodeName: string;
    introHtml: string | null;
    thumbUrl: string | null;
    imageUrl: string | null;
    mapUrl: string | null;
    bucket: string;
    latLng: { lat: number; lng: number };
  };
};

export type AudioImagesQueryVariables = Exact<{
  feedId: string;
  startTime: Date;
  endTime: Date;
  limit?: number | null | undefined;
  offset?: number | null | undefined;
}>;

export type AudioImagesQuery = {
  audioImages: {
    hasNextPage: boolean;
    results: Array<{
      id: string;
      startTime: Date;
      endTime: Date;
      status: string;
      objectPath: string | null;
      bucket: string | null;
      bucketRegion: string | null;
      feedId: string;
      imageSize: number | null;
      imageType: Types.ImageType | null;
    }> | null;
  } | null;
};

export type BoutTagsQueryVariables = Exact<{
  boutId: string;
}>;

export type BoutTagsQuery = {
  boutTags: {
    count: number | null;
    results: Array<{
      id: string;
      user: { username: string | null } | null;
      tag: {
        id: string;
        name: string;
        slug: string;
        description: string | null;
      } | null;
    }> | null;
  } | null;
};

export type BoutsQueryVariables = Exact<{
  feedId?: string | null | undefined;
  filter?: Types.BoutFilterInput | null | undefined;
  limit?: number | null | undefined;
  offset?: number | null | undefined;
  sort?:
    | Array<Types.BoutSortInput | null | undefined>
    | Types.BoutSortInput
    | null
    | undefined;
}>;

export type BoutsQuery = {
  bouts: {
    count: number | null;
    hasNextPage: boolean;
    results: Array<{
      id: string;
      name: string | null;
      category: Types.AudioCategory;
      duration: number | null;
      endTime: Date | null;
      startTime: Date;
      feed: {
        id: string;
        name: string;
        slug: string;
        nodeName: string;
        introHtml: string | null;
        thumbUrl: string | null;
        imageUrl: string | null;
        mapUrl: string | null;
        bucket: string;
        latLng: { lat: number; lng: number };
      } | null;
    }> | null;
  } | null;
};

export type CandidatesQueryVariables = Exact<{
  filter?: Types.CandidateFilterInput | null | undefined;
  limit?: number | null | undefined;
  offset?: number | null | undefined;
  sort?:
    | Array<Types.CandidateSortInput | null | undefined>
    | Types.CandidateSortInput
    | null
    | undefined;
}>;

export type CandidatesQuery = {
  candidates: {
    count: number | null;
    hasNextPage: boolean;
    results: Array<{
      id: string;
      minTime: Date;
      maxTime: Date;
      category: Types.DetectionCategory | null;
      detectionCount: number | null;
      visible: boolean | null;
      feed: { id: string; slug: string; name: string; nodeName: string };
      detections: Array<{
        id: string;
        category: Types.DetectionCategory | null;
        description: string | null;
        listenerCount: number | null;
        playlistTimestamp: number;
        playerOffset: number;
        timestamp: Date;
        visible: boolean | null;
        sourceIp: string | null;
        source: Types.DetectionSource;
        feedId: string;
      }>;
    }> | null;
  } | null;
};

export type DetectionsQueryVariables = Exact<{
  feedId?: string | null | undefined;
  filter?: Types.DetectionFilterInput | null | undefined;
  limit?: number | null | undefined;
  offset?: number | null | undefined;
  sort?:
    | Array<Types.DetectionSortInput | null | undefined>
    | Types.DetectionSortInput
    | null
    | undefined;
}>;

export type DetectionsQuery = {
  detections: {
    count: number | null;
    hasNextPage: boolean;
    results: Array<{
      id: string;
      category: Types.DetectionCategory | null;
      description: string | null;
      listenerCount: number | null;
      playlistTimestamp: number;
      playerOffset: number;
      timestamp: Date;
      visible: boolean | null;
      sourceIp: string | null;
      source: Types.DetectionSource;
      feedId: string;
      candidate: { id: string } | null;
    }> | null;
  } | null;
};

export type ListFeedStreamsQueryVariables = Exact<{
  feedId?: string | null | undefined;
  fromDateTime: Date;
  toDateTime: Date;
  dayBeforeFromDateTime: Date;
}>;

export type ListFeedStreamsQuery = {
  feedStreams: {
    count: number | null;
    results: Array<{
      id: string;
      startTime: Date | null;
      endTime: Date | null;
      duration: number | null;
      bucket: string | null;
      bucketRegion: string | null;
      cloudfrontUrl: string | null;
      playlistTimestamp: string | null;
      playlistPath: string | null;
      playlistM3u8Path: string | null;
      feedSegments: Array<{
        id: string;
        startTime: Date | null;
        endTime: Date | null;
        duration: number | null;
        bucket: string | null;
        bucketRegion: string | null;
        cloudfrontUrl: string | null;
        fileName: string;
        playlistM3u8Path: string | null;
        playlistPath: string | null;
        playlistTimestamp: string | null;
        segmentPath: string | null;
      }>;
    }> | null;
  } | null;
};

export type FeedsQueryVariables = Exact<{
  sort?:
    | Array<Types.FeedSortInput | null | undefined>
    | Types.FeedSortInput
    | null
    | undefined;
}>;

export type FeedsQuery = {
  feeds: Array<{
    id: string;
    name: string;
    slug: string;
    nodeName: string;
    imageUrl: string | null;
    thumbUrl: string | null;
    mapUrl: string | null;
    bucket: string;
    online: boolean | null;
    latLng: { lat: number; lng: number };
  }>;
};

export type NotificationsForBoutQueryVariables = Exact<{
  boutId: string;
  eventType?: Types.NotificationEventType | null | undefined;
}>;

export type NotificationsForBoutQuery = {
  notificationsForBout: Array<{
    id: string;
    eventType: Types.NotificationEventType | null;
    active: boolean | null;
    insertedAt: Date;
    targetCount: number | null;
    notifiedCount: number | null;
    notifiedCountUpdatedAt: Date | null;
    progress: number | null;
    finished: boolean | null;
  }>;
};

export type NotificationsForCandidateQueryVariables = Exact<{
  candidateId: string;
  eventType?: Types.NotificationEventType | null | undefined;
}>;

export type NotificationsForCandidateQuery = {
  notificationsForCandidate: Array<{
    id: string;
    eventType: Types.NotificationEventType | null;
    active: boolean | null;
    insertedAt: Date;
    targetCount: number | null;
    notifiedCount: number | null;
    notifiedCountUpdatedAt: Date | null;
    progress: number | null;
    finished: boolean | null;
  }>;
};

export type TagsQueryVariables = Exact<{
  limit?: number | null | undefined;
  offset?: number | null | undefined;
  filter?: Types.TagFilterInput | null | undefined;
  sort?:
    | Array<Types.TagSortInput | null | undefined>
    | Types.TagSortInput
    | null
    | undefined;
}>;

export type TagsQuery = {
  tags: {
    count: number | null;
    hasNextPage: boolean;
    results: Array<{
      id: string;
      name: string;
      description: string | null;
      slug: string;
    }> | null;
  } | null;
};

export type SearchTagsQueryVariables = Exact<{
  query: string;
}>;

export type SearchTagsQuery = {
  searchTags: Array<{
    id: string;
    name: string;
    description: string | null;
    slug: string;
  }>;
};

export type AudioImageUpdatedSubscriptionVariables = Exact<{
  feedId: string;
  startTime: Date;
  endTime: Date;
}>;

export type AudioImageUpdatedSubscription = {
  audioImageUpdated: {
    created: {
      id: string;
      startTime: Date;
      endTime: Date;
      status: string;
      objectPath: string | null;
      bucket: string | null;
      bucketRegion: string | null;
      feedId: string;
      imageSize: number | null;
      imageType: Types.ImageType | null;
    } | null;
    updated: {
      id: string;
      startTime: Date;
      endTime: Date;
      status: string;
      objectPath: string | null;
      bucket: string | null;
      bucketRegion: string | null;
      feedId: string;
      imageSize: number | null;
      imageType: Types.ImageType | null;
    } | null;
  } | null;
};

export type BoutNotificationSentSubscriptionVariables = Exact<{
  boutId: string;
}>;

export type BoutNotificationSentSubscription = {
  boutNotificationSent: {
    updated: {
      id: string;
      active: boolean | null;
      eventType: Types.NotificationEventType | null;
      progress: number | null;
      targetCount: number | null;
      finished: boolean | null;
      notifiedCount: number | null;
      notifiedCountUpdatedAt: Date | null;
      insertedAt: Date;
    } | null;
  } | null;
};

export class TypedDocumentString<TResult, TVariables>
  extends String
  implements DocumentTypeDecoration<TResult, TVariables>
{
  __apiType?: NonNullable<
    DocumentTypeDecoration<TResult, TVariables>["__apiType"]
  >;
  private value: string;
  public __meta__?: Record<string, any> | undefined;

  constructor(value: string, __meta__?: Record<string, any> | undefined) {
    super(value);
    this.value = value;
    this.__meta__ = __meta__;
  }

  override toString(): string & DocumentTypeDecoration<TResult, TVariables> {
    return this.value;
  }
}
export const AudioImagePartsFragmentDoc = new TypedDocumentString(
  `
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
    `,
  { fragmentName: "AudioImageParts" },
);
export const BoutPartsFragmentDoc = new TypedDocumentString(
  `
    fragment BoutParts on Bout {
  id
  name
  category
  duration
  endTime
  startTime
}
    `,
  { fragmentName: "BoutParts" },
);
export const CandidatePartsFragmentDoc = new TypedDocumentString(
  `
    fragment CandidateParts on Candidate {
  id
  minTime
  maxTime
  category
  detectionCount
  visible
}
    `,
  { fragmentName: "CandidateParts" },
);
export const DetectionPartsFragmentDoc = new TypedDocumentString(
  `
    fragment DetectionParts on Detection {
  id
  category
  description
  listenerCount
  playlistTimestamp
  playerOffset
  timestamp
  visible
  sourceIp
  source
  feedId
}
    `,
  { fragmentName: "DetectionParts" },
);
export const ErrorPartsFragmentDoc = new TypedDocumentString(
  `
    fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}
    `,
  { fragmentName: "ErrorParts" },
);
export const FeedPartsFragmentDoc = new TypedDocumentString(
  `
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
    `,
  { fragmentName: "FeedParts" },
);
export const FeedSegmentPartsFragmentDoc = new TypedDocumentString(
  `
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
    `,
  { fragmentName: "FeedSegmentParts" },
);
export const FeedStreamPartsFragmentDoc = new TypedDocumentString(
  `
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
    `,
  { fragmentName: "FeedStreamParts" },
);
export const ItemTagPartsFragmentDoc = new TypedDocumentString(
  `
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
    `,
  { fragmentName: "ItemTagParts" },
);
export const NotificationPartsFragmentDoc = new TypedDocumentString(
  `
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
    `,
  { fragmentName: "NotificationParts" },
);
export const SeedPartsFragmentDoc = new TypedDocumentString(
  `
    fragment SeedParts on Seed {
  id
  resource
  startTime
  endTime
  seededCount
}
    `,
  { fragmentName: "SeedParts" },
);
export const TagPartsFragmentDoc = new TypedDocumentString(
  `
    fragment TagParts on Tag {
  id
  name
  description
  slug
}
    `,
  { fragmentName: "TagParts" },
);
export const CancelCandidateNotificationsDocument = new TypedDocumentString(`
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
    `);

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

export const CancelNotificationDocument = new TypedDocumentString(`
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
    `);

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

export const CreateBoutDocument = new TypedDocumentString(`
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
    fragment BoutParts on Bout {
  id
  name
  category
  duration
  endTime
  startTime
}
fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}`);

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

export const CreateBoutTagDocument = new TypedDocumentString(`
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
    fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}
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
}`);

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

export const DeleteBoutTagDocument = new TypedDocumentString(`
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
    fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}
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
}`);

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

export const GenerateFeedSpectrogramsDocument = new TypedDocumentString(`
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
    fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}
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
}`);

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

export const NotifyConfirmedCandidateDocument = new TypedDocumentString(`
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
    `);

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

export const NotifyLiveBoutDocument = new TypedDocumentString(`
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
    `);

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

export const RegisterWithPasswordDocument = new TypedDocumentString(`
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
    `);

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

export const RequestPasswordResetDocument = new TypedDocumentString(`
    mutation requestPasswordReset($email: String!) {
  requestPasswordReset(input: {email: $email})
}
    `);

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

export const ResetPasswordDocument = new TypedDocumentString(`
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
    `);

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

export const SeedAllDocument = new TypedDocumentString(`
    mutation seedAll($startTime: DateTime!, $endTime: DateTime!) {
  seedAll(input: {startTime: $startTime, endTime: $endTime}) {
    ...SeedParts
  }
}
    fragment SeedParts on Seed {
  id
  resource
  startTime
  endTime
  seededCount
}`);

export const useSeedAllMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    SeedAllMutation,
    TError,
    SeedAllMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    SeedAllMutation,
    TError,
    SeedAllMutationVariables,
    TContext
  >({
    mutationKey: ["seedAll"],
    mutationFn: (variables?: SeedAllMutationVariables) =>
      fetcher<SeedAllMutation, SeedAllMutationVariables>(
        SeedAllDocument,
        variables,
      )(),
    ...options,
  });
};

useSeedAllMutation.getKey = () => ["seedAll"];

useSeedAllMutation.fetcher = (
  variables: SeedAllMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<SeedAllMutation, SeedAllMutationVariables>(
    SeedAllDocument,
    variables,
    options,
  );

export const SeedFeedsDocument = new TypedDocumentString(`
    mutation seedFeeds {
  seedFeeds {
    result {
      ...SeedParts
    }
    errors {
      ...ErrorParts
    }
  }
}
    fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}
fragment SeedParts on Seed {
  id
  resource
  startTime
  endTime
  seededCount
}`);

export const useSeedFeedsMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    SeedFeedsMutation,
    TError,
    SeedFeedsMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    SeedFeedsMutation,
    TError,
    SeedFeedsMutationVariables,
    TContext
  >({
    mutationKey: ["seedFeeds"],
    mutationFn: (variables?: SeedFeedsMutationVariables) =>
      fetcher<SeedFeedsMutation, SeedFeedsMutationVariables>(
        SeedFeedsDocument,
        variables,
      )(),
    ...options,
  });
};

useSeedFeedsMutation.getKey = () => ["seedFeeds"];

useSeedFeedsMutation.fetcher = (
  variables?: SeedFeedsMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<SeedFeedsMutation, SeedFeedsMutationVariables>(
    SeedFeedsDocument,
    variables,
    options,
  );

export const SeedResourceDocument = new TypedDocumentString(`
    mutation seedResource($resource: SeedResource!, $feedId: String!, $startTime: DateTime!, $endTime: DateTime!) {
  seedResource(
    input: {feedId: $feedId, resource: $resource, startTime: $startTime, endTime: $endTime}
  ) {
    result {
      ...SeedParts
    }
    errors {
      ...ErrorParts
    }
  }
}
    fragment ErrorParts on MutationError {
  code
  fields
  message
  shortMessage
  vars
}
fragment SeedParts on Seed {
  id
  resource
  startTime
  endTime
  seededCount
}`);

export const useSeedResourceMutation = <TError = unknown, TContext = unknown>(
  options?: UseMutationOptions<
    SeedResourceMutation,
    TError,
    SeedResourceMutationVariables,
    TContext
  >,
) => {
  return useMutation<
    SeedResourceMutation,
    TError,
    SeedResourceMutationVariables,
    TContext
  >({
    mutationKey: ["seedResource"],
    mutationFn: (variables?: SeedResourceMutationVariables) =>
      fetcher<SeedResourceMutation, SeedResourceMutationVariables>(
        SeedResourceDocument,
        variables,
      )(),
    ...options,
  });
};

useSeedResourceMutation.getKey = () => ["seedResource"];

useSeedResourceMutation.fetcher = (
  variables: SeedResourceMutationVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<SeedResourceMutation, SeedResourceMutationVariables>(
    SeedResourceDocument,
    variables,
    options,
  );

export const SetDetectionVisibleDocument = new TypedDocumentString(`
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
    `);

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

export const SignInWithPasswordDocument = new TypedDocumentString(`
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
    `);

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

export const SignOutDocument = new TypedDocumentString(`
    mutation signOut {
  signOut
}
    `);

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

export const SubmitDetectionDocument = new TypedDocumentString(`
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
    `);

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

export const UpdateBoutDocument = new TypedDocumentString(`
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
    fragment BoutParts on Bout {
  id
  name
  category
  duration
  endTime
  startTime
}`);

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

export const BoutDocument = new TypedDocumentString(`
    query bout($id: ID!) {
  bout(id: $id) {
    ...BoutParts
    feed {
      ...FeedParts
    }
  }
}
    fragment BoutParts on Bout {
  id
  name
  category
  duration
  endTime
  startTime
}
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
}`);

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

export const BoutExportDocument = new TypedDocumentString(`
    query boutExport($boutId: ID!) {
  bout(id: $boutId) {
    id
    exportJson
    exportJsonFileName
    exportScript
    exportScriptFileName
  }
}
    `);

export const useBoutExportQuery = <TData = BoutExportQuery, TError = unknown>(
  variables: BoutExportQueryVariables,
  options?: Omit<
    UseQueryOptions<BoutExportQuery, TError, TData>,
    "queryKey"
  > & {
    queryKey?: UseQueryOptions<BoutExportQuery, TError, TData>["queryKey"];
  },
) => {
  return useQuery<BoutExportQuery, TError, TData>({
    queryKey: ["boutExport", variables],
    queryFn: fetcher<BoutExportQuery, BoutExportQueryVariables>(
      BoutExportDocument,
      variables,
    ),
    ...options,
  });
};

useBoutExportQuery.document = BoutExportDocument;

useBoutExportQuery.getKey = (variables: BoutExportQueryVariables) => [
  "boutExport",
  variables,
];

useBoutExportQuery.fetcher = (
  variables: BoutExportQueryVariables,
  options?: RequestInit["headers"],
) =>
  fetcher<BoutExportQuery, BoutExportQueryVariables>(
    BoutExportDocument,
    variables,
    options,
  );

export const CandidateDocument = new TypedDocumentString(`
    query candidate($id: ID!) {
  candidate(id: $id) {
    ...CandidateParts
    feed {
      id
      slug
      name
      nodeName
      bucket
    }
    detections {
      ...DetectionParts
    }
  }
}
    fragment CandidateParts on Candidate {
  id
  minTime
  maxTime
  category
  detectionCount
  visible
}
fragment DetectionParts on Detection {
  id
  category
  description
  listenerCount
  playlistTimestamp
  playerOffset
  timestamp
  visible
  sourceIp
  source
  feedId
}`);

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

export const GetCurrentUserDocument = new TypedDocumentString(`
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
    `);

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

export const DetectionsCountDocument = new TypedDocumentString(`
    query detectionsCount($feedId: String!, $fromTime: DateTime!, $toTime: DateTime, $category: DetectionCategory) {
  feedDetectionsCount(
    feedId: $feedId
    fromTime: $fromTime
    toTime: $toTime
    category: $category
  )
}
    `);

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

export const FeedDocument = new TypedDocumentString(`
    query feed($slug: String!) {
  feed(slug: $slug) {
    ...FeedParts
  }
}
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
}`);

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

export const AudioImagesDocument = new TypedDocumentString(`
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
}`);

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

export const BoutTagsDocument = new TypedDocumentString(`
    query boutTags($boutId: String!) {
  boutTags(boutId: $boutId) {
    count
    results {
      ...ItemTagParts
    }
  }
}
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
}`);

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

export const BoutsDocument = new TypedDocumentString(`
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
    fragment BoutParts on Bout {
  id
  name
  category
  duration
  endTime
  startTime
}
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
}`);

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

export const CandidatesDocument = new TypedDocumentString(`
    query candidates($filter: CandidateFilterInput, $limit: Int, $offset: Int, $sort: [CandidateSortInput]) {
  candidates(filter: $filter, limit: $limit, offset: $offset, sort: $sort) {
    count
    hasNextPage
    results {
      ...CandidateParts
      feed {
        id
        slug
        name
        nodeName
      }
      detections {
        ...DetectionParts
      }
    }
  }
}
    fragment CandidateParts on Candidate {
  id
  minTime
  maxTime
  category
  detectionCount
  visible
}
fragment DetectionParts on Detection {
  id
  category
  description
  listenerCount
  playlistTimestamp
  playerOffset
  timestamp
  visible
  sourceIp
  source
  feedId
}`);

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

export const DetectionsDocument = new TypedDocumentString(`
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
      ...DetectionParts
      candidate {
        id
      }
    }
  }
}
    fragment DetectionParts on Detection {
  id
  category
  description
  listenerCount
  playlistTimestamp
  playerOffset
  timestamp
  visible
  sourceIp
  source
  feedId
}`);

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

export const ListFeedStreamsDocument = new TypedDocumentString(`
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
}`);

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

export const FeedsDocument = new TypedDocumentString(`
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
    `);

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

export const NotificationsForBoutDocument = new TypedDocumentString(`
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
    `);

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

export const NotificationsForCandidateDocument = new TypedDocumentString(`
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
    `);

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

export const TagsDocument = new TypedDocumentString(`
    query tags($limit: Int, $offset: Int, $filter: TagFilterInput, $sort: [TagSortInput]) {
  tags(limit: $limit, offset: $offset, filter: $filter, sort: $sort) {
    count
    hasNextPage
    results {
      ...TagParts
    }
  }
}
    fragment TagParts on Tag {
  id
  name
  description
  slug
}`);

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

export const SearchTagsDocument = new TypedDocumentString(`
    query searchTags($query: String!) {
  searchTags(query: $query) {
    ...TagParts
  }
}
    fragment TagParts on Tag {
  id
  name
  description
  slug
}`);

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

export const AudioImageUpdatedDocument = new TypedDocumentString(`
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
}`);
export const BoutNotificationSentDocument = new TypedDocumentString(`
    subscription boutNotificationSent($boutId: String!) {
  boutNotificationSent(boutId: $boutId) {
    updated {
      ...NotificationParts
    }
  }
}
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
}`);
