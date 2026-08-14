import { Box } from "@mui/material";
import { ReactElement } from "react";

import Banner from "@/components/Shipnoise/Banner";

function ShipnoiseLayout({ children }: { children: React.ReactNode }) {
  return (
    <Box
      sx={{
        minHeight: "100dvh",
        display: "flex",
        flexDirection: "column",
        bgcolor: "white",
      }}
    >
      <Banner />
      <Box sx={{ flexGrow: 1 }}>{children}</Box>
    </Box>
  );
}

export function getShipnoiseLayout(page: ReactElement) {
  return <ShipnoiseLayout>{page}</ShipnoiseLayout>;
}
