import React from "react"
import { Paper, Typography } from "@material-ui/core"
import styled from "styled-components"
import { useTheme } from "@material-ui/styles"

const StyledAboutContainer = styled(Paper)`
  @media screen and (min-width: 599px) {
    padding: 1rem;
    margin: 1rem 3rem 1rem 5rem;
  }

  padding: 0.5rem;
  margin: 0.1rem 0.3rem 0.1rem 0.5rem;
`

// background: ${theme.palette.primary.main};

const About = props => {
  const theme = useTheme()
  console.log("about theme", theme)
  return (
    <StyledAboutContainer elevation={0} square>
      <Typography
        variant="h6"
        style={{
          margin: "0rem 0rem 1rem 0rem"
        }}
      >
        Listen for Whales!
      </Typography>
      <Typography component="p" paragraph={true}>
        Learn what orcas sound like. Then listen live for them on underwater
        microphones (hydrophones).
      </Typography>
      <Typography component="p" paragraph={true}>
        Let us know when you hear them, or any sound you think is interesting!
        That will help researchers and stewards protect the orcas and their
        environment.
      </Typography>
      <Typography component="p" paragraph={true}>
        You can also get notified when our listeners or algorithms detect whales
        at any of our hydrophone locations.
      </Typography>
    </StyledAboutContainer>
  )
}

export default About
