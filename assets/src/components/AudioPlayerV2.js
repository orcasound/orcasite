import React from 'react'
import { Paper, Typography, Card, CardHeader } from "@material-ui/core"
import styled from "styled-components"

const AudioHeader = styled(Paper)`
  margin-left: 1rem;
`;

const CardWrapper = styled(Card)`
  display: flex;
  justify-content: center;
`;

const Audio = styled.audio`
  margin: .5rem;
`;

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
        <Audio
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3">
        </Audio>
      </CardWrapper>
      <CardWrapper>
        <Audio
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/4min-sample.mp3">
        </Audio>
      </CardWrapper>
      <CardWrapper>
        <Audio
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3">
        </Audio>
      </CardWrapper>
    </Paper>
  );
}

export default AudioPlayerV2;
