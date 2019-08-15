import React, { Component } from "react"
import { Link as RouterLink } from "react-router-dom"
import {
  Paper,
  AppBar,
  Toolbar,
  Tabs,
  Tab,
  Button,
  Typography,
  Box,
  Link,
  Grid
} from "@material-ui/core"
import styled from "styled-components"
import NotificationIcon from "@material-ui/icons/Notifications"
import FeedList from "./FeedList"

const StyledTabs = styled(Tabs)`
  display: grid;
  flex-grow: 1;
`

class SiteMenu extends Component {
  state = {
    value: 0
  }

  handleChange = (event, value) => {
    //console.log(value)
    this.setState({ value })
  }

  aboutTabSelected = () => {
    return this.state.value == 0
  }

  render() {
    return (
      <Paper elevation={0} square>
        <AppBar position="static" color="inherit">
          <Toolbar>
            <Typography component="h1" variant="h1">
              <Link
                component={RouterLink}
                to="/"
                color="inherit"
                underline="none"
                variant="inherit"
              >
                <Box ml={1} pt={3}>
                  Orcasound
                </Box>
              </Link>
            </Typography>
          </Toolbar>
        </AppBar>
        <Button
          variant="contained"
          centered="true"
          fullWidth={true}
          color="primary"
        >
          <Typography component="div">
            <Box>Get notified when there's whale activity</Box>
          </Typography>
          <Box p={0.2}>
            <NotificationIcon />
          </Box>
        </Button>
        <Tabs
          value={this.state.value}
          indicatorColor="primary"
          onChange={this.handleChange}
          centered
          scrollButtons="off"
          variant="fullWidth"
        >
          <Tab label="About" component={RouterLink} to="/" />
          <FeedList />
        </Tabs>
      </Paper>
    )
  }
}

export default SiteMenu
