import React, { Component } from "react"
import { Paper, Hidden, Grid } from "@material-ui/core"

import FeedPageV2 from "./FeedPageV2"
import PlayerV2 from "./PlayerV2"
import SiteMenu from "./SiteMenu"
import AboutV2 from "./AboutV2"
import AudioExamplesV2 from "./AudioExamplesV2"
import VerticalImageV2 from "./VerticalImageV2"

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

export default class HomeV2 extends Component {
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
                    <AboutV2 />
                    <AudioExamplesV2 />
                  </Grid>
                  <Grid item xs={12} sm={4} md={4} lg={5} xl={5}>
                    <Hidden xsDown>
                      <VerticalImageV2 />
                    </Hidden>
                  </Grid>
                </Grid>
              )}
              {feedSlug && (
                <FeedPageLayout>
                  <FeedPageV2
                    feedSlug={feedSlug}
                    onChangeFeed={this.changeFeed}
                  >
                    <PlayerLayout>
                      <PlayerV2
                        currentFeed={this.state.currentFeed}
                        key={
                          this.state.currentFeed &&
                          this.state.currentFeed.nodeName
                        }
                        autoplay={this.state.autoplay}
                      />
                    </PlayerLayout>
                  </FeedPageV2>
                </FeedPageLayout>
              )}
            </Paper>
          </ThemeProvider>
        </StylesProvider>
      </>
    )
  }
}
