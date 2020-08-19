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
  Link,
  makeStyles
} from "@material-ui/core"
import NotificationIcon from "@material-ui/icons/Notifications"
import FeedList from "./FeedList"

const useStyles = makeStyles(theme => ({
  button: {
    minWidth: "180px",
    borderRadius: 0,
    "&:hover": {
      textDecoration: "none",
      color: "#000000"
    },
    "&:active": {
      color: "#15766b"
    },
    "&:visited": {
      color: "#000000"
    }
  },
  link: {
    "&:hover": {
      textDecoration: "none",
      color: "#ffffff"
    },
    "&:active": {
      color: "#15766b"
    },
    "&:visited": {
      color: "#ffffff"
    }
  }
}))

const SiteMenu = () => {
  const classes = useStyles()
  const [value, setValue] = useState("about")

  const notificationDoc = `https://docs.google.com/forms/d/1oYSTa3QeAAG-G_eTxjabrXd264zVARId9tp2iBRWpFs/edit`

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
              className={classes.link}
            >
              <Box ml={1} pt={3}>
                <h1>Orcasound</h1>
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
        className={classes.button}
      >
        <Typography component="div">
          <Box zIndex={1}>
            <Link
              href={notificationDoc}
              target="_blank"
              rel="noopener"
              rel="noreferrer"
              color="inherit"
              variant="inherit"
              className={classes.link}
            >
              Get notified when there's whale activity
            </Link>
          </Box>
        </Typography>
        <Box p={0.2}>
          <NotificationIcon />
        </Box>
      </Button>
      <Tabs
        variant="fullWidth"
        centered
        value={value}
        indicatorColor="primary"
        onChange={handleChange}
        scrollButtons="off"
      >
        <Tab
          className={classes.button}
          value="about"
          label="About"
          component={RouterLink}
          to="/"
        />
        <Tab value="listen" label="Listen Live" component={FeedList} />
      </Tabs>
    </Paper>
  )
}

export default SiteMenu
