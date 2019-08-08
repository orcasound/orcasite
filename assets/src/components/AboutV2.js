import React from "react"
import { Paper, Typography, Grid, Box } from "@material-ui/core"

const About = props => {
  return (
    <Grid
      container
      spacing={0}
      direction="column"
      justify="flex-start"
      alignItems="flex-start"
    >
      <Grid item>
        <Typography variant="h5" component="h5">
          <Box ml={{ xs: 2, sm: 4, md: 6, lg: 8, xl: 10 }} mt={2} mr={3}>
            Listen for Whales!
          </Box>
        </Typography>
      </Grid>
      <Grid item xs={12} sm={10} md={9} lg={8} xl={6}>
        <Typography variant="body1" component="div">
          <Box ml={{ xs: 2, sm: 4, md: 6, lg: 8, xl: 10 }} mt={1.5} mr={3}>
            Learn what orcas sound like. Then listen live for them on underwater
            microphones (hydrophones).
          </Box>
        </Typography>
      </Grid>
      <Grid item xs={12} sm={10} md={9} lg={8} xl={6}>
        <Typography variant="body1" component="div">
          <Box ml={{ xs: 2, sm: 4, md: 6, lg: 8, xl: 10 }} mt={1.5} mr={3}>
            Let us know when you hear them, or any sound you think is
            interesting! That will help researchers and stewards protect the
            orcas and their environment.
          </Box>
        </Typography>
      </Grid>
      <Grid item xs={12} sm={10} md={9} lg={8} xl={6}>
        <Typography variant="body1" component="div">
          <Box ml={{ xs: 2, sm: 4, md: 6, lg: 8, xl: 10 }} mt={1.5} mr={3}>
            You can also get notified when our listeners or algorithms detect
            whales at any of our hydrophone locations.
          </Box>
        </Typography>
      </Grid>
    </Grid>
  )
}

export default About
