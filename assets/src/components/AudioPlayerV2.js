import React from "react"
import { Paper, Typography, Card } from "@material-ui/core"
import Grid from "@material-ui/core/Grid"
import Avatar from "@material-ui/core/Avatar"

import styled from "styled-components"

import equalizer from "../../static/equalizer-orca.png"
import orcalogo from "../../static/orca logo v2.5-small.png"
import wave from "../../static/wave-orca.png"

// -------------------------------------------
// Styled Components - TODO:  Move to a new file
// -------------------------------------------

const AudioContainer = styled(Paper)`
  @media screen and (min-width: 599px) {
    margin: 1rem 3rem 1rem 5rem;
  }
`

const AudioHeader = styled(Paper)`
  margin-left: 1rem;
`

const CardWrapper = styled(Card)`
  @media screen and (min-width: 599px) {
  }

  display: flex;
  justify-content: center;
  border: none;
  box-shadow: none;

  .avatar {
    margin: 0.1rem;
  }
`

const Audio = styled.audio`
  @media screen and (min-width: 599px) {
    margin: 0.5rem;
  }
`
// -------------------------------------------
// Component
// -------------------------------------------
const AudioPlayerV2 = () => {
  return (
    <AudioContainer elevation={0} square>
      <AudioHeader elevation={0} square>
        <Typography
          component="h6"
          variant="h6"
          style={{ margin: "0rem 0rem 1rem 0rem" }}
        >
          What do orcas sound like?
        </Typography>
        <Typography variant="body1">
          Here are some samples of calls, clicks, and whistles that are made by
          southern resident killer whales:
        </Typography>
      </AudioHeader>
      <CardWrapper>
        <Avatar alt="wave" src={wave} className="avatar" />
        <Typography variant="overline">Calls</Typography>
        <Audio
          id="call-examples"
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3"
        />
      </CardWrapper>
      <CardWrapper>
        <Avatar alt="wave" src={wave} className="avatar" />
        <Typography variant="overline">Clicks</Typography>
        <Audio
          id="four-minute-sample"
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/4min-sample.mp3"
        />
      </CardWrapper>
      <CardWrapper>
        <Avatar alt="wave" src={wave} className="avatar" />
        <Typography variant="overline">Whistles</Typography>
        <Audio
          id="whistle-examples"
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3"
        />
      </CardWrapper>
    </AudioContainer>
  )
}

export default AudioPlayerV2
