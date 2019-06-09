import React from "react"
import { Paper, Typography } from "@material-ui/core"


const About = () => {
  return (
    <Paper elevation={0} square>
      <Typography variant="h6"
      //className={classes.bodyHeader}
      >
        Listen for Whales!
          </Typography>
      <Typography
        component="p"
        paragraph={true}
      >
        Learn what orcas sound like. Then listen live for them on underwater
        microphones (hydrophones).
          </Typography>
      <Typography
        component="p"
        paragraph={true}
      >
        Let us know when you hear them, or any sound you think is
        interesting! That will help researchers and stewards protect the
        orcas and their environment.
          </Typography>
      <Typography
        component="p"
        paragraph={true}
      >
        You can also get notified when our listeners or algorithms detect
        whales at any of our hydrophone locations.
          </Typography>
    </Paper>
  )
}

export default About
