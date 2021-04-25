import { createMuiTheme, responsiveFontSizes } from "@material-ui/core/styles"

let theme = createMuiTheme({
  palette: {
    primary: {
      main: "#2196f3"
    },
    secondary: {
      main: "#009688"
    },
    text: {
      secondary: "#ffffff"
    }
  },
  typography: {
    htmlFontSize: 16,
    h1: {
      fontSize: 2.875,
      fontWeight: 500,
      letterSpacing: "0.067rem"
    },
    h2: {
      fontSize: "1rem",
      lineHeight: 1.75
    },
    body1: {
      fontSize: "0.875rem",
      lineHeight: 1.4,
      fontWeight: 400
    }
  },
  overrides: {
    MuiAppBar: {
      positionStatic: {
        backgroundColor: "#000000",
        color: "#ffffff",
        height: "5rem"
      }
    },
    MuiButton: {
      contained: {
        fontSize: "0.875rem"
      }
    }
  }
})

theme = responsiveFontSizes(theme)

export default theme
