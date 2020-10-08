import React from "react"
import { Typography, Box, Grid, Avatar } from "@material-ui/core"
import wave from "../../static/wave-orca.png"

class AudioExamples extends React.Component {

  constructor(props) {
    super(props);
  } 
  
  handleHomepagePlayButtonPressed() {
    gtag('event', 'play_button_pressed @ ' + this.title_str  + ' player, homepage', {'event_category' : 'custom'})
  }
  
  handleHomepagePauseButtonPressed() {
    gtag('event', 'pause_button_pressed @ ' + this.title_str  + ' player, homepage', {'event_category' : 'custom'})
  }
  
  render() {
    return (
      <Grid
        className="audio-example-section"
        component="section"
        container
        direction="column"
        justify="space-evenly"
        alignItems="flex-start"
        item
      >
        <Grid
          container
          spacing={0}
          direction="column"
          justify="flex-start"
          alignItems="flex-start"
        >
          <Grid item xs={12} md={10}>
            <Typography variant="h5" component="h5">
              <Box ml={{ xs: 3, sm: 9, md: 12, lg: 20 }} mt={{ xs: 2 }}>
                What do orcas sound like?
              </Box>
            </Typography>
          </Grid>
          <Grid item xs={12} sm={12} md={10} lg={8} xl={8}>
            <Typography variant="body1" component="div">
              <Box
                mt={{ xs: 2 }}
                mb={{ xs: 1 }}
                ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
                mr={{ xs: 3, sm: 10, md: 12 }}
                fontSize={{ xs: "0.875rem", sm: "1rem" }}
              >
                Here are some samples of calls, clicks, and whistles that are made
                by southern resident killer whales:
              </Box>
            </Typography>
          </Grid>
        </Grid>
        {this.props.example.map((example, index) => {
          return (
            <Grid
              container
              item
              direction="column"
              justify="flex-start"
              alignItems="flex-start"
              key={index}
            >
              <Grid
                item
                container
                direction="row"
                justify="center"
                alignItems="center"
              >
                <Grid item xs={2}>
                  <Box ml={{ xs: 2, sm: 9, md: 12, lg: 20 }} p={1}>
                    <Avatar alt="wave" src={wave} />
                  </Box>
                </Grid>
                <Grid item xs={10}>
                  <Typography component="div" variant="h6">
                    <Box ml={{ xs: 2, sm: 6, md: 10, lg: 12 }} pl={1}>
                      {example.title}
                    </Box>
                  </Typography>
                </Grid>
              </Grid>
              <Grid item xs={12}>
                <Box ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}>
                  <audio controls src={example.audio}
                         onPlay={this.handleHomepagePlayButtonPressed.bind({title_str: example.title})}
                         onPause={this.handleHomepagePauseButtonPressed.bind({title_str: example.title})}
                  />
                </Box>
              </Grid>
            </Grid>
          )
        })}
      </Grid>
    )
  }
}

AudioExamples.defaultProps = {
  example: [
    {
      title: "Orca Calls",
      audio:
        "http://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3"
    },
    {
      title: "Orca Clicks",
      audio:
        "http://orcasound.net/data/product/SRKW/clicks/20190705-JK_varied_clicks-10sec.mp3"
    },
    {
      title: "Orca Whistles",
      audio:
        "http://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3"
    }
  ]
}

export default AudioExamples
