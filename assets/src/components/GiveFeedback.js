import React from "react"
import { AppBar, Button, Link, Grid, makeStyles } from "@material-ui/core"

const useStyles = makeStyles(theme => ({
  appBar: {
    bottom: 0,
    backgroundColor: "none",
    width: "9.25rem"
  },
  button: {
    bottom: 0,
    background: "#000000",
    width: "9.25rem",
    height: "3rem"
  }
}))

const GiveFeedback = () => {
  const classes = useStyles()
  const feedbackLink = `https://forms.gle/wKpAnxzUh9a5LMfd7`

  return (
    <AppBar position="relative" className={classes.appBar} color="inherit">
      <Grid container direction="column" alignItems="center" justify="flex-end">
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
