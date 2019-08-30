import React from "react"
import { CardMedia, Box } from "@material-ui/core"

import OrcaImage from "../../static/spyhops_ship_updated.jpg"

const VerticalImage = () => {
  return (
    <Box className="image-container" pr={0}>
      <CardMedia
        component="img"
        image={OrcaImage}
        height="100%"
        title="Orcas with Ship"
        alt="Orcas with Ship"
      />
    </Box>
  )
}

export default VerticalImage
