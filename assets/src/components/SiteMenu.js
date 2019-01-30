import React, { Component } from "react"
import { Paper, Tabs, Tab, Button } from "@material-ui/core"

class SiteMenu extends React.Component {
  state = {
    value: 0
  }

  handleChange = (event, value) => {
    console.log(value)
    this.setState({ value })
  }

  aboutTabSelected = () => {
    return this.state.value == 0
  }
  mabyeAbout = () => {
    if (this.aboutTabSelected()) {
      return (
        <Paper>
          <p>Welcome to OrcaSound!</p>
          <p>
            Learn what orcas sound like. Then listen live for them on underwater
            microphones (hydrophones).
          </p>
          <p>
            Let us know when you hear them, or any sound you think is
            interesting! That will help researchers and stewards protect the
            orcas and their environment.
          </p>
          <p>
            You can also get notified when our listeners or algorithms detect
            whales at any of our hydrophone locations.
          </p>
        </Paper>
      )
    }
  }
  render() {
    return (
      <Paper square>
        <Paper square>Orcasound</Paper>
        <Button
          color="primary"
          variant="contained"
          centered="true"
          fullWidth={true}
          className="sitemenu-notification-link"
        >
          Get notified when there's whale activity
        </Button>
        <Tabs
          value={this.state.value}
          indicatorColor="primary"
          textColor="primary"
          onChange={this.handleChange}
          centered
        >
          <Tab label="About" />
          <Tab label="Listen Live" disabled />
        </Tabs>
        {this.mabyeAbout()}
      </Paper>
    )
  }
}

export default SiteMenu
