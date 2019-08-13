import React, { Component } from "react"
import { Paper, Hidden, Grid } from "@material-ui/core"

import FeedPage from "./FeedPage"
import Player from "./Player"
import SiteMenu from "./SiteMenu"
import About from "./About"
import AudioExamples from "./AudioExamples"
import VerticalImage from "./VerticalImage"

import { StylesProvider, ThemeProvider } from "@material-ui/styles"
import CssBaseline from "@material-ui/core/CssBaseline"
import theme from "./theme"

import styled from "styled-components"

const FeedPageLayout = styled.div`
  display: flex;
`

const PlayerLayout = styled.div`
  display: flex;
`

export default class Home extends Component {
  state = {}

  componentDidMount() {
    if (["beta", "dev", "staging"].indexOf(ENV.ENV_NAME) >= 0) {
      document.title = `Orcasound ${ENV.ENV_NAME}`
    } else {
      document.title = `Orcasound`
    }
  }

  changeFeed = currentFeed => this.setState({ currentFeed, autoplay: true })

  render() {
    console.log(theme)
    const { feedSlug } = this.props.match.params
    return (
      <>
        <StylesProvider injectFirst>
          <ThemeProvider theme={theme}>
            <CssBaseline />
            <Paper square elevation={0} component="main">
              <SiteMenu />
              {!feedSlug && (
                <Grid
                  container
                  spacing={0}
                  direction="column"
                  justify="flex-start"
                  alignItems="center"
                >
                  <Grid
                    container
                    item
                    spacing={0}
                    direction="row"
                    justify="flex-start"
                    alignItems="flex-start"
                  >
                    <Grid item sm={8}>
                      <About />
                    </Grid>
                    <Grid item sm={4}>
                      <Hidden xsDown>
                        <VerticalImage />
                      </Hidden>
                    </Grid>
                  </Grid>
                  <Grid item>
                    <AudioExamples />
                  </Grid>
                </Grid>
              )}
              {feedSlug && (
                <FeedPageLayout>
                  <FeedPage feedSlug={feedSlug} onChangeFeed={this.changeFeed}>
                    <PlayerLayout>
                      <Player
                        currentFeed={this.state.currentFeed}
                        key={
                          this.state.currentFeed &&
                          this.state.currentFeed.nodeName
                        }
                        autoplay={this.state.autoplay}
                      />
                    </PlayerLayout>
                  </FeedPage>
                </FeedPageLayout>
              )}
            </Paper>
          </ThemeProvider>
        </StylesProvider>
      </>
    )
  }
}
