import React from "react"
import NotificationIcon from "@material-ui/icons/Notifications"
import FeedbackIcon from "@material-ui/icons/Feedback"

import {
  AppBar,
  Button,
  Link,
  Grid,
  Box,
  Typography,
  Tooltip,
  Hidden,
  makeStyles,
} from "@material-ui/core"

const useStyles = makeStyles((theme) => ({
  appBar: {
    top: "auto",
    bottom: "0",
    backgroundColor: "none",
    width: "9.25rem",
  },
  notified: {
    //bottom: 0,
    width: "9rem",
    height: "3rem",
    "&:hover": {
      background: theme.palette.common.black,
    },
    "&:focus": {
      outline: "none",
    },
  },
  feedback: {
    //bottom: 0,
    background: "#000000",
    color: "#ffffff",
    width: "9rem",
    height: "3rem",
    "&:hover": {
      background: theme.palette.common.black,
    },
    "&:focus": {
      outline: "none",
    },
  },
  mobileNotified: {
    //bottom: 0,
    width: "7rem",
    height: "3rem",
    "&:hover": {
      background: theme.palette.common.black,
    },
    "&:focus": {
      outline: "none",
    },
  },
  mobileFeedback: {
    //bottom: 0,
    background: "#000000",
    color: "#ffffff",
    width: "6rem",
    height: "3rem",
    "&:hover": {
      background: theme.palette.common.black,
    },
    "&:focus": {
      outline: "none",
    },
  },
  link: {
    display: "inline-flex",
    "&:hover": {
      textDecoration: "none",
      color: "#ffffff",
    },
    "&active": {
      color: "#15766b",
    },
    "&visited": {
      color: "#ffffff",
    },
  },
  icon: {
    marginLeft: ".35rem",
  },
  mobileButtonFont: {
    fontSize: ".8rem",
  },
}))

const GiveFeedback = () => {
  const classes = useStyles()
  const notificationLink = `https://forms.gle/fn1RRK46TMwNBxGn9`
  const feedbackLink = `https://forms.gle/wKpAnxzUh9a5LMfd7`

  return (
    <Grid
      container
      direction="row"
      alignItems="flex-end"
      justify="flex-start"
      spacing={1}
    >
      {/** --- MOBILE VIEW --- */}
      <Hidden smUp>
        <Grid item>
          <Button
            className={classes.mobileNotified}
            variant="contained"
            color="primary"
          >
            <Link
              href={notificationLink}
              color="inherit"
              variant="inherit"
              target="_blank"
              rel="noopener"
              rel="noreferrer"
              className={classes.link}
            >
              <Typography variant="button" className={classes.mobileButtonFont}>
                Notifications
              </Typography>
              <Tooltip
                arrow
                title={<Typography>Receive Notifications</Typography>}
              >
                <Hidden xsUp>
                  <NotificationIcon className={classes.icon} fontSize="small" />
                </Hidden>
              </Tooltip>
            </Link>
          </Button>
        </Grid>
        <Grid item>
          <Box ml={{ xs: 0, sm: 0, md: 2, lg: 4 }}>
            <Button className={classes.mobileFeedback} variant="contained">
              <Link
                href={feedbackLink}
                color="inherit"
                variant="inherit"
                target="_blank"
                rel="noopener"
                rel="noreferrer"
                className={classes.link}
              >
                <Typography
                  variant="button"
                  className={classes.mobileButtonFont}
                >
                  Feedback
                </Typography>
                <Tooltip
                  arrow
                  title={
                    <Typography>Share Feedback on your experience</Typography>
                  }
                >
                  <Hidden xsUp>
                    <FeedbackIcon className={classes.icon} fontSize="small" />
                  </Hidden>
                </Tooltip>
              </Link>
            </Button>
          </Box>
        </Grid>
      </Hidden>
      {/** --- TABLET -> DESKTOP BREAKPOINT --- */}
      <Hidden xsDown>
        <Grid item>
          <Button
            className={classes.notified}
            variant="contained"
            color="primary"
          >
            <Link
              href={feedbackLink}
              color="inherit"
              variant="inherit"
              target="_blank"
              rel="noopener"
              rel="noreferrer"
              className={classes.link}
            >
              <Typography variant="button">Notifications</Typography>
              <Tooltip
                arrow
                title={<Typography>Receive Notifications</Typography>}
              >
                <NotificationIcon className={classes.icon} fontSize="small" />
              </Tooltip>
            </Link>
          </Button>
        </Grid>
        <Grid item>
          <Box ml={{ xs: 0, sm: 0, md: 2, lg: 4 }}>
            <Button className={classes.feedback} variant="contained">
              <Link
                href={feedbackLink}
                color="inherit"
                variant="inherit"
                target="_blank"
                rel="noopener"
                rel="noreferrer"
                className={classes.link}
              >
                <Typography variant="button">Feedback</Typography>
                <Tooltip
                  arrow
                  title={
                    <Typography>Share Feedback on your experience</Typography>
                  }
                >
                  <FeedbackIcon className={classes.icon} fontSize="small" />
                </Tooltip>
              </Link>
            </Button>
          </Box>
        </Grid>
      </Hidden>
    </Grid>
  )
}

export default GiveFeedback
