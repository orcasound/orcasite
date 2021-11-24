import { createTheme } from '@mui/material/styles'

// Module augmentation for custom colors
// Based on https://mui.com/customization/palette/#adding-new-colors
declare module '@mui/material/styles' {
  interface Palette {
    accent1: Palette['primary']
    accent2: Palette['primary']
    accent3: Palette['primary']
    accent4: Palette['primary']
  }
  interface PaletteOptions {
    accent1: PaletteOptions['primary']
    accent2: PaletteOptions['primary']
    accent3: PaletteOptions['primary']
    accent4: PaletteOptions['primary']
  }
}

const theme = createTheme({
  palette: {
    primary: {
      main: '#080d26',
    },
    secondary: {
      main: '#1b2b7b',
    },
    accent1: {
      main: '#008bdf',
    },
    accent2: {
      main: '#7c7cfe',
    },
    accent3: {
      main: '#4760fe',
    },
    accent4: {
      main: '#f79234',
    },
    error: {
      main: '#e9222f',
    },
    warning: {
      main: '#f79234',
    },
    info: {
      main: '#008bdf',
    },
    success: {
      main: '#00c56f',
    },
  },
})

export default theme
