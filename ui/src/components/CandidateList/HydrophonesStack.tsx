import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import React from "react";

import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";

export const HydrophonesStack = () => {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const { feeds } = useData();

  const HydrophoneList = ({ feeds }: { feeds: Feed[] }) => {
    return feeds.map((feed) => {
      return (
        <Box key={feed.id}>
          <div>{feed.name}</div>
          <div>{feed.id}</div>
        </Box>
      );
    });
  };

  return (
    <Stack>
      <Box sx={{ paddingTop: "1.5rem", overflow: mdDown ? "auto" : "initial" }}>
        <HydrophoneList feeds={feeds} />
      </Box>
    </Stack>
  );
};
