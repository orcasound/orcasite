<template>
  <div id="app">
    <h1>{{ timestamp }}</h1>
    <h3><a v-bind:href="manifestUri">{{ manifestUri }}</a></h3>
    <h3><a v-bind:href="awsConsoleUri">{{ awsConsoleUri }}</a></h3>
    <video id="video"
           width="640"
           controls autoplay>
    </video>
  </div>
</template>

<script>
import gql from 'graphql-tag'
import { shaka } from 'shaka-player/dist/shaka-player.compiled.debug'
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
    manifestUri: function () {
      return `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/rpi_seattle/dash/${this.timestamp}/live.mpd`;
    },
    awsConsoleUri: function () {
      return `https://s3.console.aws.amazon.com/s3/buckets/dev-streaming-orcasound-net/rpi_seattle/dash/${this.timestamp}/`;
    }
  },
  methods: {
    initApp: function () {
      // Install built-in polyfills to patch browser incompatibilities.
      shaka.polyfill.installAll();

      // Check to see if the browser supports the basic APIs Shaka needs.
      if (shaka.Player.isBrowserSupported()) {
        // Everything looks good!
        this.initPlayer();
      } else {
        // This browser does not have the minimum set of APIs we need.
        console.error('Browser not supported!');
        // TODO: Falback to HLS
      }
    },
    initPlayer: function () {
      // Create a Player instance.
      var video = document.getElementById('video');
      var player = new shaka.Player(video);

      // Attach player to the window to make it easy to access in the JS console.
      window.player = player;
      this.player = player;

      // Listen for error events.
      player.addEventListener('error', this.onErrorEvent);

      player.configure({
        manifest: {
          retryParameters: {
            timeout: 60000,
            baseDelay: 1000,
            maxAttempts: 5
          }
        },
        streaming: {
          bufferBehind: 300,
          bufferingGoal: 300,
          rebufferingGoal: 5,
          retryParameters: {
            timeout: 60000,
            baseDelay: 1000,
            maxAttempts: 30
          }
        }
      });

      this.fetchTimestamp();
      setInterval(this.fetchTimestamp, 10000);
    },
    loadManifest: function () {
      // Try to load a manifest.
      // This is an asynchronous process.
      this.player.load(this.manifestUri).then(function() {
        // This runs if the asynchronous load is successful.
        console.log('The video has now been loaded!');
      }).catch(this.onLoadError);  // onLoadError is executed if the asynchronous load fails.
    },
    onErrorEvent: function (event) {
      // Extract the shaka.util.Error object from the event.
      this.onError(event.detail);
    },
    onLoadError: function (error) {
      this.onError(error);
      setTimeout(this.loadManifest, 5000);
    },
    onError: function (error) {
      // Log the error.
      console.error('Error code', error.code, 'object', error);      
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
          console.log("New stream instance: " + self.manifestUri)
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