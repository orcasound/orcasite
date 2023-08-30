// Self-hosted fonts using https://fontsource.org/
import "@fontsource/mukta";
import "@fontsource/montserrat";

import { createTheme } from "@mui/material/styles";

// Module augmentation for custom colors
// Based on https://mui.com/customization/palette/#adding-new-colors
declare module "@mui/material/styles" {
  interface Palette {
    base: Palette["primary"];
    accent1: Palette["primary"];
    accent2: Palette["primary"];
    accent3: Palette["primary"];
    accent4: Palette["primary"];
  }
  interface PaletteOptions {
    base: PaletteOptions["primary"];
    accent1: PaletteOptions["primary"];
    accent2: PaletteOptions["primary"];
    accent3: PaletteOptions["primary"];
    accent4: PaletteOptions["primary"];
  }
}

declare module "@mui/material/AppBar" {
  interface AppBarPropsColorOverrides {
    base: true;
  }
}

declare module "@mui/material/Fab" {
  interface FabPropsColorOverrides {
    base: true;
  }
}

// Fonts with fallbacks
const mukta = "Mukta, Helvetica, Arial, sans-serif";
const montserrat = "Montserrat, Helvetica, Arial, sans-serif";

// Hack to get access to theme helpers
// see https://github.com/mui/material-ui/issues/35895#issuecomment-1401579770
const helperTheme = createTheme();

const theme = createTheme({
  palette: {
    primary: {
      main: "#1b2b7b",
    },
    secondary: {
      main: "#080d26",
    },
    base: helperTheme.palette.augmentColor({
      color: {
        main: "#080d26",
        contrastText: "#ffffff",
      },
    }),
    accent1: {
      main: "#008bdf",
    },
    accent2: {
      main: "#7c7cfe",
    },
    accent3: {
      main: "#4760fe",
    },
    accent4: {
      main: "#f79234",
    },
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
      fontFamily: montserrat,
    },
    body2: {
      fontFamily: montserrat,
    },
  },
});

theme.components = {
  ...theme.components,
  MuiCssBaseline: {
    styleOverrides: `
      html, body, #__next {
        height: 100dvh;
      }
    `,
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
        backgroundColor: theme.palette.base.main,
      },
    },
  },
  MuiBottomNavigationAction: {
    styleOverrides: {
      root: {
        color: theme.palette.base.contrastText,
      },
    },
  },
};

export default theme;
