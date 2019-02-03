import React, { Component } from "react"
import { Paper, Tabs, Tab, Button, Typography, Grid } from "@material-ui/core"
import NotificationIcon from "@material-ui/icons/Notifications"
// import ArrowDropDown from "@material-ui/icons/ArrowDropDown"
import { withStyles } from "@material-ui/core/styles"

const styles = theme => ({
  header: {
    background: "#000000",
    height: 80
  },
  h1: {
    color: "#ffffff",
    fontSize: 30,
    letterSpacing: 1.07,
    paddingLeft: 20,
    paddingTop: 18,
  },
  button: {
    fontSize: 10,
    color: '#ffffff',
    opacity: .87,
    textAlign: 'left',
    height: 40
  },
  notificationIcon: {
    fontSize: 16,
  },
  label: {
    fontSize: 14,
    textAlign: 'center',
  },
  labelIcon: {
    fontSize: 14,
    textAlign: 'center',
  },
  bodyHeader: {
    marginLeft: 15,
    marginRight: 15,
    paddingTop: 18
  },
  bodyText: {
    marginLeft: 15,
    marginRight: 15,
    marginTop: 15
  }
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
        const { classes } = this.props;
        return (
          <Paper>
            <Typography
              variant="h6"
              className={classes.bodyHeader}
            //style={{ paddingTop: 18 }}
            >
              Welcome to OrcaSound!
            </Typography>
            <Typography
              component="p"
              paragraph={true}
              className={classes.bodyText}
            >
              Learn what orcas sound like. Then listen live for them on underwater
              microphones (hydrophones).
            </Typography>
            <Typography
              component="p"
              paragraph={true}
              className={classes.bodyText}
            >
              Let us know when you hear them, or any sound you think is
              interesting! That will help researchers and stewards protect the
              orcas and their environment.
            </Typography>
            <Typography
              component="p"
              paragraph={true}
              className={classes.bodyText}
            >
              You can also get notified when our listeners or algorithms detect
              whales at any of our hydrophone locations.
            </Typography>
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
            className={classes.header}
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
            className={classes.button}
          // Do we need this className?  
          // className="sitemenu-notification-link"
          >
            Get notified when there's whale activity
            <NotificationIcon
              className={classes.notificationIcon}
            />
          </Button>
          <Tabs
            value={this.state.value}
            variant="fullWidth"
            indicatorColor="primary"
            textColor="primary"
            onChange={this.handleChange}
          //centered
          >
            <Tab
              label="About"
              fullWidth={true}
              classes={{
                label: classes.label
              }}
            />
            <Tab
              label="Listen Live"
              disabled
              //icon={<ArrowDropDown />}
              fullWidth={true}
              classes={{
                labelIcon: classes.labelIcon,
              }}
            >
            </Tab>
          </Tabs>
          {this.mabyeAbout()}
        </Paper>
      )
    }
  }
)
