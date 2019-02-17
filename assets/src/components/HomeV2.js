import React, { Component } from "react"
import SiteMenu from "./SiteMenu"
import AudioPlayerV2 from "./AudioPlayerV2"
import { Button, Paper } from "@material-ui/core"
import { createMuiTheme, MuiThemeProvider, withStyles } from '@material-ui/core/styles'
import CssBaseline from "@material-ui/core/CssBaseline"
import { blue, black } from '@material-ui/core/colors'
import color from "@material-ui/core/colors/indigo";
import { CallMissedSharp } from "@material-ui/icons";

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
);

const styles = {
  paper: {
    height: '100%'
  }
};

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
              elevation={0}
              className={CallMissedSharp.paper}
            >
              <SiteMenu />
              <AudioPlayerV2 />
            </Paper>
          </MuiThemeProvider>
        </CssBaseline>
      </>
    )
  }
}
