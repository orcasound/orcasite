import React from "react"
import { CardMedia, Grid, Box } from "@material-ui/core"
import OrcaImage from "../../static/spyhops_ship_updated.jpg"

const VerticalImageV2 = () => {
  return (
    <Grid container>
      <Grid item>
        <Box className="image-container">
          <CardMedia component="img" src={OrcaImage} height="100%" />
        </Box>
      </Grid>
    </Grid>
  )
}

export default VerticalImageV2
