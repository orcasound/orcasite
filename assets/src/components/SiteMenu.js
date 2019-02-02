import React, { Component } from "react"
import { Paper, Tabs, Tab, Button, Typography } from "@material-ui/core"
import { withStyles } from "@material-ui/core/styles"

const styles = theme => ({
  root: {
  },
  background: {
    background: "#000000",
    height: 80
  },
  h1: {
    color: "#ffffff",
    fontFamily: 'Roboto-Medium',
    fontSize: 30,
    letterSpacing: 1.07,
    // marginRight: 203,
    paddingLeft: 20,
    paddingTop: 10,
  },
});

export default withStyles(styles)(
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
      const { classes } = this.props;

      return (
        <Paper
          square
        >
          <Paper
            square
            className={classes.background}
            elevation={1}
          >
            <Typography
              component="h1"
              className={classes.h1}
              align="left"
            >
              Orcasound
            </Typography>
          </Paper>
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
)

//export default SiteMenu
