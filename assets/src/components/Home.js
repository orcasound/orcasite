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
    const { feedSlug } = this.props.match.params
    return (
      <>
        <StylesProvider injectFirst>
          <ThemeProvider theme={theme}>
            <CssBaseline />
            <Paper square elevation={0}>
              <SiteMenu />
              {!feedSlug && (
                <Grid
                  container
                  spacing={0}
                  direction="row"
                  justify="flex-start"
                  alignItems="flex-start"
                >
                  <Grid item xs={12} sm={8} md={8} lg={7} xl={7}>
                    <About />
                    <AudioExamples />
                  </Grid>
                  <Grid item xs={12} sm={4} md={4} lg={5} xl={5}>
                    <Hidden xsDown>
                      <VerticalImage />
                    </Hidden>
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
