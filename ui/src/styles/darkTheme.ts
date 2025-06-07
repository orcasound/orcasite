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
      dark: "#cccccc",
      contrastText: "#1b2b7b",
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
        main: "#3D3F49",
        // main: "#323f52",
        contrastText: "#ffffff",
      },
      name: "darkgrey",
    }),
    action: {
      hover: "rgba(255, 255, 255, 0.08)", // subtle white-on-dark hover
      disabledBackground: "rgba(255, 255, 255, 0.12)", // muted, readable disabled bg
      disabled: "rgba(255, 255, 255, 0.3)", // disabled text/icons
      selected: "rgba(255, 255, 255, 0.16)", // optional: for selected items
      focus: "rgba(255, 255, 255, 0.2)", // optional: focus ring bg if needed
    },
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
      color: darkTheme.palette.text.primary,
      // underline: "hover",
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
  MuiButton: {
    styleOverrides: {
      root: {
        "&:hover": {
          cursor: "pointer",
        },
      },
      outlined: {
        color: darkTheme.palette.text.primary,
        borderColor: "rgba(255,255,255,0.5)",
        "&:hover": {
          borderColor: "rgba(255,255,255,0.8)",
          backgroundColor: "rgba(255,255,255,0.04)", // subtle hover bg
        },
        "&:disabled": {
          color: darkTheme.palette.text.disabled,
          borderColor: "rgba(255,255,255,0.2)",
        },
        "&.Mui-focused": {
          borderColor: darkTheme.palette.primary.main,
        },
      },
      contained: {
        color: darkTheme.palette.background.default,
        backgroundColor: darkTheme.palette.primary.main,
        "&:hover": {
          backgroundColor: darkTheme.palette.primary.main,
        },
        "&:disabled": {
          backgroundColor: darkTheme.palette.action.disabledBackground,
          color: darkTheme.palette.text.disabled,
        },
        "&.Mui-focused": {
          boxShadow: `0 0 0 3px ${darkTheme.palette.primary.light}33`, // soft focus ring
        },
      },
      // You can also style the default (text) variant if needed
      text: {
        color: darkTheme.palette.text.primary,
        "&:hover": {
          backgroundColor: darkTheme.palette.action.hover,
        },
      },
    },
  },
  MuiCssBaseline: {
    styleOverrides: {
      a: {
        color: darkTheme.palette.accent3.main,
        "&:hover": {
          color: darkTheme.palette.accent4.main,
        },
      },
    },
  },
};

export default darkTheme;
