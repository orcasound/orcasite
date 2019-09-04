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

class Home extends Component {
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
      <>
        <StylesProvider injectFirst>
          <ThemeProvider theme={theme}>
            <CssBaseline />
            <Paper
              square
              elevation={0}
              component="div"
              style={{ position: "relative" }}
            >
              <SiteMenu />
              {!feedSlug && (
                <Grid
                  component="main"
                  container
                  spacing={0}
                  direction="row"
                  justify="center"
                  alignItems="flex-start"
                >
                  <Grid
                    container
                    item
                    spacing={0}
                    direction="column"
                    justify="flex-start"
                    alignItems="flex-start"
                    sm={8}
                  >
                    <Grid item>
                      <About />
                    </Grid>
                    <Grid item>
                      <AudioExamples />
                    </Grid>
                  </Grid>
                  <Grid item sm={4}>
                    <Hidden xsDown>
                      <VerticalImage />
                    </Hidden>
                  </Grid>
                  <Grid item>
                    <GiveFeedback />
                  </Grid>
                </Grid>
              )}
              {feedSlug && (
                <Grid
                  container
                  direction="column"
                  justify="center"
                  alignItems="center"
                >
                  <Grid item>
                    <FeedPage
                      key={feedSlug}
                      feedSlug={feedSlug}
                      onChangeFeed={this.changeFeed}
                    />
                  </Grid>
                  <Grid item>
                    <GiveFeedback />
                  </Grid>
                </Grid>
              )}
            </Paper>
          </ThemeProvider>
        </StylesProvider>
      </>
    )
  }
}

export default Home
