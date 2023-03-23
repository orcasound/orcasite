import React from "react"
import { Typography, Grid, Box } from "@material-ui/core"

const About = props => {
  return (
    <Grid
      className="about-section"
      component="section"
      container
      spacing={1}
      direction="column"
      justify="flex-start"
      alignItems="flex-start"
    >
      <Grid item xs={12} md={10} lg={8}>
        <Typography variant="h5" component="h5">
          <Box
            mt={{ xs: 2, sm: 4 }}
            mr={{ xs: 3, sm: 10, md: 12 }}
            ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
          >
            Listen for Whales!
          </Box>
        </Typography>
      </Grid>
      <Grid item xs={12} md={10} lg={8}>
        <Typography variant="body1" component="div">
          <Box
            mt={{ xs: 1, sm: 1 }}
            mr={{ xs: 3, sm: 10, md: 12 }}
            ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
            fontSize={{ xs: "0.875rem", sm: "1rem" }}
          >
            Learn what orcas sound like. Then listen live for them on underwater
            microphones (hydrophones).
          </Box>
        </Typography>
      </Grid>
      <Grid item xs={12} md={10} lg={8}>
        <Typography variant="body1" component="div">
          <Box
            mt={{ xs: 0, sm: 1 }}
            mr={{ xs: 3, sm: 10, md: 12 }}
            ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
            fontSize={{ xs: "0.875rem", sm: "1rem" }}
          >
            Let us know when you hear them, or any sound you think is
            interesting! That will help researchers and stewards protect the
            orcas and their environment.
          </Box>
        </Typography>
      </Grid>
      <Grid item xs={12} md={10} lg={8}>
        <Typography variant="body1" component="div">
          <Box
            mt={{ xs: 0, sm: 1 }}
            mr={{ xs: 3, sm: 10, md: 12 }}
            ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
            fontSize={{ xs: "0.875rem", sm: "1rem" }}
          >
            You can also get notified when our listeners or algorithms detect
            whales at any of our hydrophone locations.
          </Box>
        </Typography>
      </Grid>
    </Grid>
  )
}

export default About
