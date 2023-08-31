import { Theme } from "@mui/material";

export const displayMobileOnly = { display: { xs: "block", sm: "none" } };
export const displayDesktopOnly = { display: { xs: "none", sm: "block" } };

export const mobileOnly = (theme: Theme) => theme.breakpoints.down("sm");
export const desktopOnly = (theme: Theme) => theme.breakpoints.up("sm");
