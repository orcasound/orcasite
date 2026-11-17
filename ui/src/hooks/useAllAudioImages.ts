import { useQuery } from "@tanstack/react-query";

import { fetcher } from "@/graphql/client";
import {
  AudioImagesDocument,
  AudioImagesQuery,
  AudioImagesQueryVariables,
} from "@/graphql/generated";
import { fetchAllPages } from "@/utils/fetchAllPages";

type AudioImageResult = NonNullable<
  NonNullable<AudioImagesQuery["audioImages"]>["results"]
>[number];

/**
 * Every audio image for a feed in a time range.
 *
 * The server returns at most 1,000 images at a time, and a range of about
 * three hours holds more than that, so this asks for page after page.
 */
export function useAllAudioImages(variables: {
  feedId: string;
  startTime: Date;
  endTime: Date;
}) {
  return useQuery({
    queryKey: ["audioImages", "all", variables],
    queryFn: () =>
      fetchAllPages<AudioImageResult>(async (offset, limit) => {
        const data = await fetcher<AudioImagesQuery, AudioImagesQueryVariables>(
          AudioImagesDocument,
          { ...variables, offset, limit },
        )();
        return {
          results: data.audioImages?.results ?? [],
          hasNextPage: data.audioImages?.hasNextPage ?? false,
        };
      }),
  });
}
