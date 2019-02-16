import React from 'react';
import PropTypes from 'prop-types';
import { withStyles } from '@material-ui/core/styles';
import { Paper, Typography, Card, } from "@material-ui/core"
//import CardHeader from '@material-ui/core/CardHeader';
//import CardContent from '@material-ui/core/CardContent';
//import CardMedia from '@material-ui/core/CardMedia';

const styles = {
  playerTitle: {
    marginLeft: 15,
    marginRight: 15
  },
  card: {
    display: 'flex',
  },
  details: {
    display: 'flex',
    flexDirection: 'column',
  },
  content: {
    flex: '1 0 auto',
  },
  cover: {
    width: 151,
  },
  controls: {
    display: 'flex',
    alignItems: 'center',
    // paddingLeft: theme.spacing.unit,
    // paddingBottom: theme.spacing.unit,
  },
  playIcon: {
    height: 38,
    width: 38,
  },
};

function AudioPlayerV2(props) {
  const { classes } = props;

  return (
    <Paper elevation={0} square>
      <Typography
        className={classes.playerTitle}
        component="h6"
        variant="h6"
      >
        What do orcas sound like?
        </Typography>
      <Typography
        className={classes.playerTitle}
        variant="body1"
      >
        Here are some samples of calls, clicks, and whistles that are made by southern resident killer whales:
        </Typography>
      <Card>
        <Typography>Calls</Typography>
        <audio
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3">
          Your browser does not support the
              <code>audio</code> element.
            </audio>
      </Card>
      <Card>
        <Typography>Clicks</Typography>
        <audio
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/4min-sample.mp3">
          Your browser does not support the
          <code>audio</code> element.
        </audio>
      </Card>
      <Card>
        <Typography>Whistles</Typography>
        <audio
          controls
          src="http://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3">
          Your browser does not support the
          <code>audio</code> element.
        </audio>
      </Card>

    </Paper>
  );
}

AudioPlayerV2.propTypes = {
  classes: PropTypes.object.isRequired,
};

export default withStyles(styles)(AudioPlayerV2);
