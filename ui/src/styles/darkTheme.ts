// Self-hosted fonts using https://fontsource.org/
import "@fontsource/mukta";
import "@fontsource/montserrat";

import { createTheme } from "@mui/material/styles";

import theme from "./theme";

// Fonts with fallbacks
const mukta = "Mukta, Helvetica, Arial, sans-serif";
// const montserrat = "Montserrat, Helvetica, Arial, sans-serif";

// Hack to get access to theme helpers
// see https://github.com/mui/material-ui/issues/35895#issuecomment-1401579770
// const helperTheme = createTheme();

const darkTheme = createTheme({
  ...theme,
  palette: {
    ...theme.palette,
    mode: "dark",
    primary: {
      ...theme.palette.primary,
      main: "#ffffff",
    },
    background: {
      default: "#080d26",
      paper: "#141931",
    },
    text: {
      primary: "#ffffff",
      secondary: "rgba(255,255,255,.75)",
    },
    base: theme.palette.augmentColor({
      color: {
        // main: "#3D3F49",
        main: "#323f52",
        contrastText: "#ffffff",
      },
      name: "darkgrey",
    }),
    accent1: theme.palette.augmentColor({
      color: {
        main: "#002f49",
      },
      name: "darkslateblue",
    }),
    accent2: theme.palette.augmentColor({
      color: {
        main: "#9b9b9b",
      },
      name: "middlegrey",
    }),
    accent3: theme.palette.augmentColor({
      color: {
        main: "#a4d3d1",
      },
      name: "lightaqua",
    }),
    accent4: theme.palette.augmentColor({
      color: {
        main: "#258dad",
      },
      name: "aqua",
    }),
    error: {
      main: "#e9222f",
    },
    warning: {
      main: "#f79234",
    },
    info: {
      main: "#008bdf",
    },
    success: {
      main: "#00c56f",
    },
  },
  typography: {
    fontFamily: mukta,
    body1: {
      fontFamily: mukta,
      lineHeight: "1.2",
    },
    body2: {
      fontFamily: mukta,
    },
  },
});

darkTheme.components = {
  ...darkTheme.components,
  MuiSvgIcon: {
    styleOverrides: {
      root: {
        color: darkTheme.palette.text.primary,
      },
    },
  },
  MuiSelect: {
    styleOverrides: {
      icon: {
        color: darkTheme.palette.text.primary,
      },
    },
  },
  MuiOutlinedInput: {
    styleOverrides: {
      root: {
        "&.Mui-focused .MuiOutlinedInput-notchedOutline": {
          borderColor: darkTheme.palette.text.primary,
        },
      },
    },
  },
  MuiAppBar: {
    defaultProps: {
      color: "base",
    },
  },
  MuiLink: {
    defaultProps: {
      // TODO: Remove the `.main` once this issue is fixed in MUI
      // https://github.com/mui-org/material-ui/issues/29596
      color: "accent3.main",
    },
  },
  MuiBottomNavigation: {
    styleOverrides: {
      root: {
        backgroundColor: darkTheme.palette.base.main,
      },
    },
  },
  MuiBottomNavigationAction: {
    styleOverrides: {
      root: {
        color: darkTheme.palette.base.contrastText,
      },
    },
  },
};

export default darkTheme;
