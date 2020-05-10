import React, { Component } from "react"
import { Paper, Hidden, Grid, AppBar } from "@material-ui/core"

import FeedPage from "./FeedPage"
import SiteMenu from "./SiteMenu"
import About from "./About"
import AudioExamples from "./AudioExamples"
import VerticalImage from "./VerticalImage"
import GiveFeedback from "./GiveFeedback"
import FeedList from "./FeedList"

import { StylesProvider, ThemeProvider, withStyles } from "@material-ui/styles"
import CssBaseline from "@material-ui/core/CssBaseline"
import theme from "./theme"

const styles = (theme) => ({
  appBar: {
    display: "flex",
    height: "3.75rem",
    // justifyContent: "space-between",
    alignItems: "center",
    background: "#777",
  },
  innerAppBar: {
    height: "100%",
    alignItems: "center",
    justifyContent: "space-between",
    margin: "0 0.5rem 0 0.5rem",
  },
  item: {
    margin: "0 0.5rem 0 0.5rem",
  },
})

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
    const { classes } = this.props
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
              <AppBar position="relative" className={classes.appBar}>
                <Grid container className={classes.innerAppBar}>
                  <Grid item className={classes.item}>
                    <GiveFeedback />
                  </Grid>

                  <Grid item className={classes.item}>
                    <FeedList />
                  </Grid>
                </Grid>
              </AppBar>

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
                      <AudioExamples />
                    </Grid>
                  </Grid>
                  <Grid item sm={4}>
                    <Hidden xsDown>
                      <VerticalImage />
                    </Hidden>
                  </Grid>
                  {/**
                    <Grid item>
                    <GiveFeedback />
                    </Grid>
                */}
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
                      feedSlug={feedSlug}
                      onChangeFeed={this.changeFeed}
                    />
                  </Grid>
                  {/**
                    <Grid item>
                    <GiveFeedback />
                    </Grid>
                */}
                </Grid>
              )}
            </Paper>
          </ThemeProvider>
        </StylesProvider>
      </>
    )
  }
}

export default withStyles(styles)(Home)
