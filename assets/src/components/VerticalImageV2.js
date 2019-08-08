import React from "react"
import { CardMedia, Grid, Box } from "@material-ui/core"

import OrcaImage from "../../static/spyhops_ship_updated.jpg"

const VerticalImageV2 = () => {
  return (
    <Grid container>
      <Grid item>
        <Box className="image-container" pr={0}>
          <CardMedia
            component="img"
            image={OrcaImage}
            height="100%"
            title="Orcas with Ship"
            alt="Orcas with Ship"
          />
        </Box>
      </Grid>
    </Grid>
  )
}

export default VerticalImageV2
