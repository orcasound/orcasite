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

class LearnPageRoot extends Component {

  componentDidMount() {
    if (["beta", "dev", "staging"].indexOf(ENV.ENV_NAME) >= 0) {
      document.title = `Orcasound ${ENV.ENV_NAME}`
    } else {
      document.title = `Orcasound`
    }
  }

  render() {
    return (
      <>
        <h1>You have reached the learn page!</h1>
      </>
    )
  }
}

export default LearnPageRoot
