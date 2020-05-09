import React from "react"
import { AppBar, Button, Link, Grid, Box, makeStyles } from "@material-ui/core"

const useStyles = makeStyles((theme) => ({
  appBar: {
    top: "auto",
    bottom: "0",
    backgroundColor: "none",
    width: "9.25rem",
  },
  notified: {
    bottom: 0,
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
    bottom: 0,
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
  link: {
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
}))

const GiveFeedback = () => {
  const classes = useStyles()
  const feedbackLink = `https://forms.gle/wKpAnxzUh9a5LMfd7`

  return (
    <Grid
      container
      direction="row"
      alignItems="flex-end"
      justify="flex-start"
      spacing={1}
    >
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
            Get Notified
          </Link>
        </Button>
      </Grid>
      <Grid item>
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
            Give Feedback
          </Link>
        </Button>
      </Grid>
    </Grid>
  )
}

export default GiveFeedback
