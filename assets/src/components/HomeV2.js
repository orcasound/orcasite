import React, { Component } from "react"
import SiteMenu from "./SiteMenu"
import { Button, Paper } from "@material-ui/core"
import { createMuiTheme, MuiThemeProvider, withStyles } from '@material-ui/core/styles'
import CssBaseline from "@material-ui/core/CssBaseline"
import { blue, black } from '@material-ui/core/colors'
import color from "@material-ui/core/colors/indigo";

const theme = createMuiTheme(
  {
    /* config */
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
    spacing: {
      unit: 10 // overriding default theme value of 8
    }
  }
);

console.log(theme);

export default class HomeV2 extends Component {
  state = {}

  componentDidMount() {
    document.title = `Orcasound`
  }

  render() {
    return (
      <>
        <CssBaseline>
          <MuiThemeProvider theme={theme}>
            <Paper
              square
            >
              <SiteMenu />
            </Paper>
          </MuiThemeProvider>
        </CssBaseline>
      </>
    )
  }
}
