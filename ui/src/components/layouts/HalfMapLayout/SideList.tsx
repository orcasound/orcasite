import { Box } from "@mui/material";

import CandidatesTabs from "@/components/CandidateList/DesktopTabs";

export function SideList() {
  return (
    <Box
      className="side-list"
      sx={{
        borderRightColor: "divider",
        borderRightStyle: "solid",
        borderRightWidth: 1,
        width: "45%",
        overflow: "auto",
      }}
    >
      <CandidatesTabs />
    </Box>
  );
}
