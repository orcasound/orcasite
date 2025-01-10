import { AppBar, Toolbar } from "@mui/material";

import Desktop from "./Desktop";
import Mobile from "./Mobile";

export default function Header({
  onBrandClick,
}: {
  onBrandClick?: () => void;
}) {
  return (
    <AppBar
      position="static"
      sx={{
        // Keep header above the side drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
      }}
    >
      <Toolbar>
        <Mobile onBrandClick={onBrandClick} />
        <Desktop />
      </Toolbar>
    </AppBar>
  );
}
