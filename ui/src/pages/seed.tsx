import {
  Box,
  Button,
  Card,
  Container,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  Snackbar,
  Typography,
} from "@mui/material";
import { addMinutes, formatDuration, subHours } from "date-fns";
import _ from "lodash";
import Head from "next/head";
import { useEffect, useState } from "react";

import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import {
  FeedQuery,
  SeedFeedsResult,
  SeedResource,
  SeedResourceResult,
  useFeedsQuery,
  useSeedFeedsMutation,
  useSeedResourceMutation,
} from "@/graphql/generated";
import { NextPageWithLayout } from "@/pages/_app";

const SeedPage: NextPageWithLayout = () => {
  const feedsQuery = useFeedsQuery();
  const feeds = feedsQuery.data?.feeds ?? [];
  const resources = Object.values(SeedResource);

  const [startTime, setStartTime] = useState(() => subHours(new Date(), 1));
  const [selectedFeed, setSelectedFeed] = useState<string | undefined>();
  const intervals = [1, 5, 10, 15, 30, 45, 60]; // minutes
  const [selectedInterval, setSelectedInterval] = useState(60);
  const [selectedResource, setSelectedResource] = useState(resources[0]);

  useEffect(() => {
    if (selectedFeed === undefined && feeds.length > 0) {
      setSelectedFeed(feeds[0].id);
    }
  }, [feeds]);

  const [seedForm, setSeedForm] = useState({
    isSaving: false,
    errors: {},
    saved: false,
    message: "",
  });
  const onSuccess = (response: SeedFeedsResult | SeedResourceResult) => {
    const { result, errors } = response;
    if (errors && errors.length > 0) {
      console.error(errors);
      setSeedForm((form) => ({
        ...form,
        isSaving: false,
        errors: {
          ...form.errors,
          ...Object.fromEntries(
            errors.map(({ code, message }) => [code, message] as const),
          ),
        },
      }));
    } else if (result) {
      setSeedForm((form) => ({
        ...form,
        isSaving: false,
        saved: true,
        message: `${result.seededCount} ${lowerCaseResource(result.resource)}s seeded`,
      }));
    }
  };
  const seedFeedsMutation = useSeedFeedsMutation({
    onSuccess: ({ seedFeeds }: { seedFeeds: SeedFeedsResult }) => {
      onSuccess(seedFeeds);
      feedsQuery.refetch();
    },
  });
  const seedResourceMutation = useSeedResourceMutation({
    onSuccess: ({ seedResource }: { seedResource: SeedResourceResult }) => {
      onSuccess(seedResource);
    },
  });

  const handleSubmit = () => {
    if (selectedResource === "FEED") {
      seedFeedsMutation.mutate({});
    } else if (selectedFeed) {
      seedResourceMutation.mutate({
        resource: selectedResource,
        feedId: selectedFeed,
        startTime: startTime,
        endTime: addMinutes(startTime, selectedInterval),
      });
    }
  };

  return (
    <div>
      <Head>
        <title>Seed resources | Orcasound</title>
      </Head>

      <main>
        <Container sx={{ mb: 5 }}>
          <h1>Seed resources from production</h1>
          <p>Pull data from production</p>
          <Box
            my={2}
            display="flex"
            flexDirection="row"
            gap={2}
            alignItems={"center"}
          >
            <FormControl sx={{ minWidth: "100px" }}>
              <InputLabel>Resource</InputLabel>

              {selectedResource && (
                <Select
                  label="Resource"
                  value={selectedResource}
                  onChange={(event) =>
                    event.target.value &&
                    setSelectedResource(event.target.value as SeedResource)
                  }
                >
                  {resources.map((resource) => (
                    <MenuItem value={resource} key={resource}>
                      {titleizeResource(resource)}s
                    </MenuItem>
                  ))}
                </Select>
              )}
            </FormControl>
            {selectedResource !== "FEED" && selectedFeed && (
              <>
                <FormControl sx={{ minWidth: "100px" }}>
                  <InputLabel>Feed</InputLabel>
                  <Select
                    label="Feed"
                    value={selectedFeed}
                    onChange={(event) =>
                      event.target.value && setSelectedFeed(event.target.value)
                    }
                  >
                    {feeds.map((feed: FeedQuery["feed"]) => (
                      <MenuItem value={feed.id} key={feed.id}>
                        {feed.name}
                      </MenuItem>
                    ))}
                  </Select>
                </FormControl>
                <Box>
                  <Typography fontSize={12}>Starting</Typography>
                  <input
                    type="datetime-local"
                    name="startTime"
                    value={toLocalISOString(startTime)}
                    onChange={(event) =>
                      setStartTime(new Date(event.target.value))
                    }
                    style={{
                      padding: "10px 14px",
                      border: "1px solid #ccc",
                      borderRadius: "4px",
                    }}
                  />
                </Box>
                <Box>
                  <FormControl>
                    <InputLabel>For</InputLabel>
                    <Select
                      label="For"
                      value={selectedInterval}
                      onChange={(event) =>
                        setSelectedInterval(Number(event.target.value))
                      }
                    >
                      {intervals.map((interval) => (
                        <MenuItem key={interval} value={interval}>
                          {formatDuration({ minutes: interval })}
                        </MenuItem>
                      ))}
                      <MenuItem></MenuItem>
                    </Select>
                  </FormControl>
                </Box>
              </>
            )}
            <Button
              sx={{ ml: "auto" }}
              size="large"
              variant="contained"
              disabled={seedForm.isSaving}
              {...(seedForm.isSaving ? { startIcon: <LoadingSpinner /> } : {})}
              onClick={handleSubmit}
            >
              Submit
            </Button>
          </Box>
          {seedForm.message && (
            <Snackbar
              message={seedForm.message}
              open={seedForm.saved}
              onClose={() => setSeedForm((form) => ({ ...form, saved: false }))}
              autoHideDuration={5000}
            />
          )}
          <Box display="flex" flexDirection="column" gap={3}>
            {resources.map((resource) => (
              <Card key={resource} sx={{ p: 4 }}>
                <Typography variant="subtitle1">
                  {titleizeResource(resource)}
                </Typography>
                <Typography variant="body1">{description(resource)}</Typography>
              </Card>
            ))}
          </Box>
        </Container>
      </main>
    </div>
  );
};

function description(resource: string) {
  const mapping: { [key: string]: string } = {
    AUDIO_IMAGE: "Spectrograms or any other type of image representing audio.",
    FEED: "Hydrophones",
    FEED_STREAM:
      "Represents an m3u8 file in S3. Whenever a feed restarts, a new m3u8 file is created",
    FEED_SEGMENT:
      "Represents a single .ts file from a feed (usually 10 second). A feed_stream has many feed_segments. Used to track timestamps for querying audio from S3",
    DETECTION:
      "A single user-submitted report of tagged audio (whale, vessel, other)",
    CANDIDATE:
      "Groups one or many detections based on whether detections of the same category (whale, vessel, other) are within 3 minutes of each other",
    BOUT: "A moderator-generated time interval for a feed where there's a specific category of audio going on. Usually 10-90 minutes long.",
  };

  return mapping[resource];
}

function titleizeResource(resource: string) {
  return resource[0] + _.replace(resource, "_", " ").toLowerCase().slice(1);
}

function lowerCaseResource(resource: string) {
  return _.replace(resource, "_", " ").toLowerCase();
}

function toLocalISOString(date: Date) {
  const utcSeconds = date.getTime() / 1000;

  const isoString = new Date(
    utcSeconds * 1000 - new Date().getTimezoneOffset() * 60000,
  )
    .toISOString()
    .substring(0, 16);

  return isoString;
}

SeedPage.getLayout = getSimpleLayout;

export default SeedPage;
