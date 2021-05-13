import React, { useState } from "react"
import { Link as RouterLink } from "react-router-dom"
import {
  Paper,
  Grid,
  AppBar,
  Toolbar,
  Tabs,
  Tab,
  Button,
  Typography,
  Box,
  Link,
  makeStyles,
} from "@material-ui/core"
import NotificationIcon from "@material-ui/icons/Notifications"
import FeedList from "./FeedList"

const useStyles = makeStyles((theme) => ({
  button: {
    minWidth: "180px",
    borderRadius: 0,
    "&:hover": {
      textDecoration: "none",
      color: "#000000",
    },
    "&:active": {
      color: "#15766b",
    },
    "&:visited": {
      color: "#000000",
    },
  },
  link: {
    "&:hover": {
      textDecoration: "none",
      color: "#ffffff",
    },
    "&:active": {
      color: "#15766b",
    },
    "&:visited": {
      color: "#ffffff",
    },
  },
}))

const SiteMenu = () => {
  const classes = useStyles()
  const [value, setValue] = useState("about")

  const handleChange = (e, newValue) => {
    setValue(newValue)
  }

  return (
    <Paper elevation={0} square>
      <AppBar position="static" color="inherit">
        <Toolbar>
          <Grid container justify="space-between" alignItems="center">
            <Grid item>
              <Typography component="h1" variant="h1" className={classes.h1}>
                <Link
                  component={RouterLink}
                  to="/"
                  color="inherit"
                  underline="none"
                  variant="inherit"
                  className={classes.link}
                >
                  <Box ml={0} pt={3}>
                    Orcasound
                  </Box>
                </Link>
              </Typography>
            </Grid>
          </Grid>
        </Toolbar>
      </AppBar>
    </Paper>
  )
}

export default SiteMenu
