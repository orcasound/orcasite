import CloseIcon from "@mui/icons-material/Close";
import {
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  Paper,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
  Typography,
} from "@mui/material";
import type { StaticImageData } from "next/legacy/image";
import { useState } from "react";

import type { DetectionCategory, Feed } from "@/graphql/generated";
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

  const categoryButtons: {
    id: DetectionCategory;
    iconImage: StaticImageData;
  }[] = [
    { id: "WHALE", iconImage: whaleFlukeIconImage },
    { id: "VESSEL", iconImage: vesselIconImage },
    { id: "OTHER", iconImage: wavesIconImage },
  ];

  return (
    <>
      <Box onClick={handleClickOpen}>{children}</Box>
      <Dialog
        open={open}
        onClose={handleClose}
        aria-labelledby="form-dialog-title"
      >
        <IconButton
          aria-label="close"
          onClick={handleClose}
          sx={{
            position: "absolute",
            right: 8,
            top: 8,
            color: (theme) => theme.palette.grey[500],
          }}
        >
          <CloseIcon />
        </IconButton>

        {!submitted ? (
          <DialogTitle id="form-dialog-title">
            Report what you heard
          </DialogTitle>
        ) : (
          <>
            <DialogTitle
              id="form-dialog-title"
              variant="h4"
              mt={8}
              align="center"
              sx={{
                color: (theme) => theme.palette.primary.main,
              }}
            >
              Thank you for reporting
            </DialogTitle>
            <Typography variant="body1" mb={2} mx={8} align="center">
              Check out our reports to see how your support contributed to our
              mission for marine conservation.
            </Typography>
          </>
        )}

        {!submitted && (
          <DialogContent>
            <ToggleButtonGroup
              value={category}
              exclusive
              onChange={handleCategoryChange}
              size="large"
              aria-label="Report sound"
              fullWidth
              orientation="horizontal"
              sx={{
                marginY: [1, 4],
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
                      marginX: [1, 2],
                      marginY: [1, 0],
                      borderRadius: 1,
                      overflow: "visible",
                      border: "solid 2px",
                      backgroundColor: "primary.main",
                      color: "background.default",
                      borderColor: "transparent",
                      ":hover": {
                        borderColor: "primary.main",
                      },
                    },
                    "&&.Mui-selected": {
                      border: "solid 2px",
                      borderColor: "primary.main",
                      backgroundColor: "text.secondary",
                    },
                  }}
                >
                  <DetectionCategoryButton icon={iconImage} title={id} />
                </ToggleButton>
              ))}
            </ToggleButtonGroup>
            <TextField
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
          <>
            <DialogActions sx={{ justifyContent: "center", mb: 10 }}>
              <Button
                onClick={handleClose}
                color="primary"
                variant="outlined"
                sx={{ py: 2, px: [3, 6], mx: 2 }}
              >
                BACK
              </Button>
              <Button
                href="/reports"
                target="_blank"
                color="primary"
                variant="contained"
                sx={{ py: 2, px: [3, 6], mx: 2 }}
              >
                SEE REPORTS
              </Button>
            </DialogActions>
          </>
        )}
      </Dialog>
    </>
  );
}
