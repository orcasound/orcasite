import { Box, Container, Stack, Theme, useMediaQuery } from "@mui/material";
import React from "react";

import { CandidatesStack } from "./CandidatesStack";
import { HydrophonesStack } from "./HydrophonesStack";
import { VisualizationsStack } from "./VisualizationsStack";

export default function DesktopTabs({ tabIndex }: { tabIndex: number }) {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  interface TabPanelProps {
    children?: React.ReactNode;
    index: number;
    value: number;
  }

  function DesktopTabPanel(props: TabPanelProps) {
    const { children, value, index, ...other } = props;
    return (
      <div
        role="tabpanel"
        hidden={value !== index}
        id={`simple-tabpanel-${index}`}
        aria-labelledby={`simple-tab-${index}`}
        style={{ marginTop: "24px" }}
        {...other}
      >
        {value === index && <Box sx={{ p: 0 }}>{children}</Box>}
      </div>
    );
  }

  const tabPanels = (
    <Stack>
      <DesktopTabPanel value={tabIndex} index={0}>
        <HydrophonesStack />
      </DesktopTabPanel>
      <DesktopTabPanel value={tabIndex} index={1}>
        <CandidatesStack />
      </DesktopTabPanel>
      <DesktopTabPanel value={tabIndex} index={2}>
        <VisualizationsStack />
      </DesktopTabPanel>
    </Stack>
  );

  return (
    <Container
      maxWidth="xl"
      sx={{
        px: { xs: 1, sm: 2, md: 3 },
        pb: "200px",
      }}
    >
      <Box
        sx={{
          borderBottom: 0,
          borderColor: "accent1",
          marginTop: mdDown ? 0 : "1rem",
        }}
      >
        {tabPanels}
      </Box>
    </Container>
  );
}
