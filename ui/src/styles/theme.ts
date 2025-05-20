// Self-hosted fonts using https://fontsource.org/
import "@fontsource/mukta";
import "@fontsource/montserrat";
import "@fontsource/noto-mono"; // Monospace

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

declare module "@mui/material/styles" {
  interface TypographyVariants {
    monospace: React.CSSProperties;
  }

  // allow configuration using `createTheme`
  interface TypographyVariantsOptions {
    monospace?: React.CSSProperties;
  }
}

// Update the Typography's variant prop options
declare module "@mui/material/Typography" {
  interface TypographyPropsVariantOverrides {
    monospace: true;
  }
}

// Fonts with fallbacks
const mukta = "Mukta, Helvetica, Arial, sans-serif";
const montserrat = "Montserrat, Helvetica, Arial, sans-serif";
const notoMono = "'Noto Mono', monospace;"; // pretty good

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
    accent1: helperTheme.palette.augmentColor({
      color: {
        main: "#002f49",
      },
      name: "accent1",
    }),
    accent2: helperTheme.palette.augmentColor({
      color: {
        main: "#9b9b9b",
      },
      name: "accent2",
    }),
    accent3: helperTheme.palette.augmentColor({
      color: {
        main: "#a4d3d1",
      },
      name: "accent3",
    }),
    accent4: helperTheme.palette.augmentColor({
      color: {
        main: "#258dad",
      },
      name: "accent4",
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
      fontFamily: montserrat,
    },
    body2: {
      fontFamily: montserrat,
    },
    monospace: {
      fontFamily: notoMono,
    },
  },
});

theme.components = {
  ...theme.components,
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
