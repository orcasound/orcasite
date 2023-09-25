import {
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Paper,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
} from "@mui/material";
import { useState } from "react";

import { DetectionCategory, Feed } from "@/graphql/generated";
import { useSubmitDetectionMutation } from "@/graphql/generated";
import vesselIconImage from "@/public/icons/vessel-purple.svg";
import wavesIconImage from "@/public/icons/water-waves-blue.svg";
import whaleFlukeIconImage from "@/public/icons/whale-fluke-gray.svg";
import { analytics } from "@/utils/analytics";

import DetectionCategoryButton from "./DetectionCategoryButton";

export default function DetectionDialog({
  children,
  feed: { id: feedId, slug },
  timestamp,
  isPlaying,
  getPlayerTime,
  listenerCount,
}: {
  children: React.ReactNode;
  feed: Pick<Feed, "id" | "slug">;
  timestamp?: number;
  isPlaying: boolean;
  getPlayerTime?: () => number | undefined;
  listenerCount: number;
}) {
  const [open, setOpen] = useState(false);
  const [submitted, setSubmitted] = useState(false);
  const [category, setCategory] = useState<DetectionCategory>();
  const [description, setDescription] = useState("");
  const [playerOffset, setPlayerOffset] = useState<number>();
  const [playlistTimestamp, setPlaylistTimestamp] = useState<number>();

  const submitDetection = useSubmitDetectionMutation({
    onSuccess: () => {
      setSubmitted(true);
    },
  });

  const handleClickOpen = () => {
    setSubmitted(false);
    setDescription("");
    setCategory(undefined);

    setPlayerOffset(getPlayerTime?.());
    if (timestamp) {
      setPlaylistTimestamp(timestamp);
    }

    setOpen(true);
    analytics.detection.dialogOpened(slug);
  };

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) =>
    setDescription(e.target.value);

  const handleCategoryChange = (
    e: React.MouseEvent<HTMLElement>,
    newCategory: DetectionCategory,
  ) => {
    setCategory(newCategory);
  };

  const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.which === 13) {
      onDetect();
    }
  };

  const handleClose = () => {
    setOpen(false);
    analytics.detection.dialogClosed(slug);
  };

  const onDetect = () => {
    if (
      feedId &&
      playlistTimestamp &&
      isPlaying &&
      playerOffset !== undefined &&
      category
    ) {
      submitDetection.mutate({
        feedId,
        playlistTimestamp,
        playerOffset,
        description,
        category,
        listenerCount,
      });
      analytics.detection.submitted(slug);
    }
  };

  const categoryButtons = [
    { id: DetectionCategory.Orca, iconImage: whaleFlukeIconImage },
    { id: DetectionCategory.Vessel, iconImage: vesselIconImage },
    { id: DetectionCategory.Other, iconImage: wavesIconImage },
  ];

  return (
    <>
      <Box onClick={handleClickOpen}>{children}</Box>
      <Dialog
        open={open}
        onClose={handleClose}
        aria-labelledby="form-dialog-title"
      >
        <DialogTitle id="form-dialog-title">
          {!submitted ? "Report what you heard" : "Thanks for submitting!"}
        </DialogTitle>
        {!submitted && (
          <DialogContent>
            <ToggleButtonGroup
              value={category}
              exclusive
              onChange={handleCategoryChange}
              size="large"
              aria-label="Report sound"
              fullWidth
              sx={{
                marginY: 4,
              }}
            >
              {categoryButtons.map(({ id, iconImage }) => (
                <ToggleButton
                  value={id}
                  aria-label={id}
                  key={id}
                  component={Paper}
                  sx={{
                    "&&&": {
                      marginX: 5,
                      borderRadius: 1,
                      ":hover": {
                        borderColor: "primary.main",
                      },
                    },
                    "&&.Mui-selected": {
                      border: "solid 2px",
                      borderColor: "primary.main",
                    },
                  }}
                >
                  <DetectionCategoryButton icon={iconImage} title={id} />
                </ToggleButton>
              ))}
            </ToggleButtonGroup>
            <TextField
              autoFocus
              margin="dense"
              placeholder="Describe what you heard (optional)"
              type="text"
              fullWidth
              onChange={handleChange}
              onKeyDown={handleKeyDown}
              sx={{
                marginY: 2,
              }}
            />
          </DialogContent>
        )}
        {!submitted ? (
          <DialogActions>
            <Button onClick={handleClose} color="primary">
              CANCEL
            </Button>
            <Button
              onClick={onDetect}
              color="primary"
              variant="outlined"
              disabled={!category}
            >
              SUBMIT
            </Button>
          </DialogActions>
        ) : (
          <DialogActions>
            <Button onClick={handleClose} color="primary">
              CLOSE
            </Button>
          </DialogActions>
        )}
      </Dialog>
    </>
  );
}
