<template>
  <div id="app">
    <h1>{{ timestamp }}</h1>
    <h3><a v-bind:href="hlsUri">{{ hlsUri }}</a></h3>
    <h3><a v-bind:href="awsConsoleUri">{{ awsConsoleUri }}</a></h3>
    <audio id="audio"
           controls autoplay>
    </audio>
  </div>
</template>

<script>
import gql from 'graphql-tag'
import Hls from 'hls.js'
export default {
  name: 'app',
  data () {
    return {
      msg: 'Welcome to Your Vue.js & Phoenix & GraphQL App',
      timestamp: '',
      timestampURI: 'https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/rpi_seattle/latest.txt'
    }
  },
  mounted() {
    document.addEventListener('DOMContentLoaded', this.initApp);
    console.log(this);
  },
  computed: {
    dashUri: function () {
      return `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/rpi_seattle/dash/${this.timestamp}/live.mpd`;
    },
    hlsUri: function () {
      return `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/rpi_seattle/hls/${this.timestamp}/live.m3u8`;
    },
    awsConsoleUri: function () {
      return `https://s3.console.aws.amazon.com/s3/buckets/dev-streaming-orcasound-net/rpi_seattle/hls/${this.timestamp}/`;
    }
  },
  methods: {
    initApp: function () {
      // Check to see if the browser supports hls
      this.initPlayer();
    },
    initPlayer: function () {
      // Create a Player instance.
      var audio = document.getElementById('audio');

      var hls = new Hls();


      // Attach player to the window to make it easy to access in the JS console.
      window.hls = hls;
      this.hls = hls;

      this.fetchTimestamp();
      setInterval(this.fetchTimestamp, 10000);
    },
    loadManifest: function () {
      hls.loadSource(this.hlsUri);
      hls.attachMedia(audio);
      hls.config.liveSyncDurationCount = 5;
      hls.on(Hls.Events.MANIFEST_PARSED,function() {
        audio.play();
      });
    },
    fetchTimestamp: function () {
      var xhr = new XMLHttpRequest()
      var self = this
      xhr.open('GET', this.timestampURI)
      xhr.onload = function () {
        var timestamp = xhr.responseText.trim();
        console.log("Latest timestamp: " + timestamp);
        if (timestamp != self.timestamp) {
          self.timestamp = timestamp;
          console.log("New stream instance: " + self.hlsUri)
          self.loadManifest();
        }
      }
      xhr.send()
    }
  },
  apollo: {
    // Apollo specific options
    // Here, we use gql to describe the data we want: a user with ID 1, and 
    // Apollo will assign the result of that query to the 'user' key in data.
    user: gql`{
      user(id: "1"){ 
        name
      }
    }`,
  }
}
</script>

<style lang="scss">
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
h1, h2 {
  font-weight: normal;
}
audio {
  width: 400px;
  margin-top: 20px;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>