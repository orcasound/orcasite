import React, { Component } from "react"
import SiteMenu from "./SiteMenu"
import AudioPlayerV2 from "./AudioPlayerV2"
import { Button, Paper } from "@material-ui/core"
import CssBaseline from "@material-ui/core/CssBaseline"
import { createMuiTheme, MuiThemeProvider } from "@material-ui/core/styles"
import JssProvider from "react-jss/lib/JssProvider"
import { create } from "jss"
import { createGenerateClassName, jssPreset } from "@material-ui/core/styles"
import styled, { ThemeProvider } from 'styled-components'
import { blue, black } from "@material-ui/core/colors"

const theme = createMuiTheme(
  {
    /* change default theme options below */
    /* gets merged into custom style objects */
    /* using latest version of typography */
    typography: {
      useNextVariants: true,
    },
    palette: {
      primary: {
        main: '#2196f3',
      },
      secondary: {
        main: '#009688',
      }
    },
  }
)


const generateClassName = createGenerateClassName();
const jss = create({
  ...jssPreset(),
  // Define a custom insertion for injecting the JSS styles in the DOM
  insertionPoint: 'jss-insertion-point',
});

export default class HomeV2 extends Component {
  state = {}

  componentDidMount() {
    document.title = `Orcasound`
  }

  render() {
    const { classes } = this.props

    return (
      <>
        <JssProvider jss={jss} generateClassName={generateClassName}>
          <MuiThemeProvider theme={theme}>
            <Paper
              square
              elevation={0}
            >
              <SiteMenu />
              <AudioPlayerV2 />
            </Paper>
          </MuiThemeProvider>
        </JssProvider>
      </>
    )
  }
}
