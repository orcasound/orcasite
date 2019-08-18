import React, { useState } from "react"
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
  Link
} from "@material-ui/core"
import NotificationIcon from "@material-ui/icons/Notifications"
import FeedList from "./FeedList"

const SiteMenu = () => {
  const [value, setValue] = useState(0)

  const handleChange = (e, newValue) => {
    setValue(newValue)
  }

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
        centered
        variant="fullWidth"
        value={value}
        indicatorColor="primary"
        onChange={handleChange}
        scrollButtons="off"
      >
        <Tab label="About" component={RouterLink} to="/" />
        <Tab label="Listen Live" component={FeedList} />
      </Tabs>
    </Paper>
  )
}

export default SiteMenu
