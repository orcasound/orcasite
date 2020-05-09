import React from "react"
import { AppBar, Button, Link, Grid, makeStyles } from "@material-ui/core"
import ReactGA from 'react-ga'

const useStyles = makeStyles(theme => ({
  appBar: {
    top: "auto",
    bottom: "0",
    backgroundColor: "none",
    width: "9.25rem"
  },
  button: {
    bottom: 0,
    background: theme.palette.common.black,
    width: "9.25rem",
    height: "3rem",
    position: "fixed",
    borderRadius: ".5rem .5rem 0rem 0rem",
    opacity: ".8",
    "&:hover": {
      background: theme.palette.common.black
    },
    "&:focus": {
      outline: "none"
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

const GiveFeedback = () => {
  const classes = useStyles()
  const feedbackLink = `https://forms.gle/wKpAnxzUh9a5LMfd7`

  return (
    <AppBar position="relative" className={classes.appBar} color="inherit">
      <Grid
        container
        direction="column"
        alignItems="flex-start"
        justify="flex-end"
      >
        <Grid item>
          <Button
            className={classes.button}
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
              onClick={() => ReactGA.event({category: "Give Feedback", action: "Button clicked"})}
            >
              Give Feedback
            </Link>
          </Button>
        </Grid>
      </Grid>
    </AppBar>
  )
}

export default GiveFeedback
