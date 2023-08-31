import { Theme } from "@mui/material";

export const displayMobile = { display: { xs: "block", sm: "none" } };
export const displayDesktop = { display: { xs: "none", sm: "block" } };

export const mobileOnly = (theme: Theme) => theme.breakpoints.down("sm");
export const desktopOnly = (theme: Theme) => theme.breakpoints.up("sm");
