import React, { Component } from "react"
import SiteMenu from "./SiteMenu"
import { Button, Paper } from "@material-ui/core"

export default class HomeV2 extends Component {
  state = {}

  componentDidMount() {
      document.title = `Orcasound`
  }

  render() {
    return (
      <Paper square>
        <SiteMenu />
      </Paper>
    )
  }
}
