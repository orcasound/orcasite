import React, { Component } from "react"
import { Typography, Box, Grid, Avatar } from "@material-ui/core"
import wave from "../../static/wave-orca.png"

class AudioExamples extends Component {
  state = {
    example: [
      {
        title: "Orca Calls",
        audio:
          "http://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3"
      },
      {
        title: "Orca Clicks",
        audio:
          "http://www.orcasound.net/data/product/SRKW/orcasite/4min-sample.mp3"
      },
      {
        title: "Orca Whistles",
        audio:
          "http://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3"
      }
    ]
  }

  render() {
    return (
      <Grid
        className="audio-example-section"
        component="section"
        container
        spacing={0}
        direction="column"
        justify="space-evenly"
        alignItems="center"
        item
        xs={10}
        sm={12}
      >
        <Grid
          container
          spacing={0}
          direction="column"
          justify="center"
          alignItems="flex-start"
        >
          <Grid item xs={12} sm={10} md={9} lg={8} xl={6}>
            <Typography variant="h5" component="h5">
              <Box ml={{ xs: 2, sm: 20, md: 6, lg: 8, xl: 2 }} mt={2}>
                What do orcas sound like?
              </Box>
            </Typography>
          </Grid>
          <Grid item xs={12} sm={10} md={9} lg={8} xl={6}>
            <Typography variant="body1" component="div">
              <Box ml={{ xs: 2, sm: 8, md: 6, lg: 8, xl: 15 }} mt={1.5}>
                Here are some samples of calls, clicks, and whistles that are
                made by southern resident killer whales:
              </Box>
            </Typography>
          </Grid>
        </Grid>
        {this.state.example.map((example, index) => {
          return (
            <Grid
              container
              item
              xs={12}
              sm={6}
              md={6}
              lg={6}
              xl={4}
              direction="column"
              justify="flex-start"
              alignItems="center"
              key={index}
            >
              <Grid
                item
                xs={12}
                sm={12}
                md={12}
                lg={12}
                xl={12}
                container
                direction="row"
                justify="center"
                alignItems="center"
              >
                <Grid item xs={2} sm={2} md={2} lg={2} xl={1}>
                  <Box ml={{ xs: 2, sm: 2, md: 4, lg: 8, xl: 12 }} p={1}>
                    <Avatar alt="wave" src={wave} />
                  </Box>
                </Grid>
                <Grid item xs={10} sm={10} md={10} lg={10} xl={10}>
                  <Typography component="div" variant="h6">
                    <Box ml={{ xs: 2, sm: 4, md: 6, lg: 8, xl: 15 }} pl={1}>
                      {example.title}
                    </Box>
                  </Typography>
                </Grid>
              </Grid>
              <Grid item xs={12} sm={12} md={12} lg={10} xl={10}>
                <Box ml={{ xs: 0, sm: 2, md: 4, lg: 6, xl: 2 }}>
                  <audio controls src={example.audio} />
                </Box>
              </Grid>
            </Grid>
          )
        })}
      </Grid>
    )
  }
}

export default AudioExamples
