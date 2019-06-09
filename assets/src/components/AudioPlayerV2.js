import React from 'react'
import { Paper, Typography, Card } from "@material-ui/core"
import styled from "styled-components"

// -------------------------------------------
// Styled Components - TODO:  Move to a new file
// -------------------------------------------

export const AudioHeader = styled(Paper)`
  margin-left: 1rem;
`

export const CardWrapper = styled(Card)`
  display: flex;
  justify-content: center;
`

export const Audio = styled.audio`
  margin: .5rem;
`
// -------------------------------------------
// Component
// -------------------------------------------
const AudioPlayerV2 = () => {
  return (
    <Paper elevation={0} square>
      <AudioHeader elevation={0} square>
        <Typography
          component="h6"
          variant="h6"
        >
          What do orcas sound like?
        </Typography>
        <Typography
          variant="body1"
        >
          Here are some samples of calls, clicks, and whistles that are made by southern resident killer whales:
        </Typography>
      </AudioHeader>
      <CardWrapper>
        <Audio id="call-examples"
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3">
        </Audio>
      </CardWrapper>
      <CardWrapper>
        <Audio id="four-minute-sample"
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/4min-sample.mp3">
        </Audio>
      </CardWrapper>
      <CardWrapper>
        <Audio id="whistle-examples"
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3">
        </Audio>
      </CardWrapper>
    </Paper>
  )
}

export default AudioPlayerV2
