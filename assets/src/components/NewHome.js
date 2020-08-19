import React, { Component } from "react"
import { Paper, Hidden, Grid } from "@material-ui/core"

import FeedPage from "./FeedPage"
import SiteMenu from "./SiteMenu"
import About from "./About"
import AudioExamples from "./AudioExamples"
import VerticalImage from "./VerticalImage"
import GiveFeedback from "./GiveFeedback"

import { StylesProvider, ThemeProvider } from "@material-ui/styles"
import CssBaseline from "@material-ui/core/CssBaseline"
import theme from "./theme"

import NewHomeSiteMenu from "./NewHomeSiteMenu"

class NewHome extends Component {
  state = {}

  componentDidMount() {
    if (["beta", "dev", "staging"].indexOf(ENV.ENV_NAME) >= 0) {
      document.title = `Orcasound ${ENV.ENV_NAME}`
    } else {
      document.title = `Orcasound`
    }
  }

  render() {
    const { feedSlug } = this.props.match.params

    return (
        <StylesProvider injectFirst>
          <ThemeProvider theme={theme}>
            <CssBaseline />
            <Paper
              square
              elevation={0}
              component="div"
              style={{ position: "relative" }}>
              <NewHomeSiteMenu />
            </Paper>
          </ThemeProvider>
        </StylesProvider>
    )
  }
}

export default NewHome




