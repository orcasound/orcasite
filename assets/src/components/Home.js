import React, { Component } from "react"
import { Paper, Hidden, Grid, Button, Box } from "@material-ui/core"

import FeedPage from "./FeedPage"
import Player from "./Player"
import SiteMenu from "./SiteMenu"
import About from "./About"
import AudioExamples from "./AudioExamples"
import VerticalImage from "./VerticalImage"
import GiveFeedback from "./GiveFeedback"

import { StylesProvider, ThemeProvider, withTheme } from "@material-ui/styles"
import CssBaseline from "@material-ui/core/CssBaseline"
import theme from "./theme"

import styled from "styled-components"

const FeedPageLayout = styled.div`
  display: flex;
`

const PlayerLayout = styled.div`
  display: flex;
`

const StyledBody = styled.body`
  position: relative;
`

const StyledButton = styled.button`
  background-color: ${props => props.backgroundColor}
  color: ${props => props.textColor};
`

class Home extends Component {
  constructor(props) {
    super(props)
    this.state = {}
  }

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
            <Paper
              square
              elevation={0}
              component="main"
              style={{ position: "relative" }}
            >
              <SiteMenu />
              {!feedSlug && (
                <Grid
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
                      feedSlug={feedSlug}
                      onChangeFeed={this.changeFeed}
                    >
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
