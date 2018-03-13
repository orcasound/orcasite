<template>
  <div id="app">
    <video id="video"
           width="640"
           poster="//shaka-player-demo.appspot.com/assets/poster.jpg"
           controls autoplay>
    </video>
  </div>
</template>

<script>
import gql from 'graphql-tag'
import shaka from 'shaka-player'
export default {
  name: 'app',
  data () {
    return {
      msg: 'Welcome to Your Vue.js & Phoenix & GraphQL App',
      timestamp: "",
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
      }
    },
    initPlayer: function () {
      // Create a Player instance.
      var video = document.getElementById('video');
      var player = new shaka.Player(video);

      // Attach player to the window to make it easy to access in the JS console.
      window.player = player;

      // Listen for error events.
      player.addEventListener('error', this.onErrorEvent);

      // Try to load a manifest.
      // This is an asynchronous process.
      player.load(this.manifestUri).then(function() {
        // This runs if the asynchronous load is successful.
        console.log('The video has now been loaded!');
      }).catch(onError);  // onError is executed if the asynchronous load fails.
    },
    onErrorEvent: function (event) {
      // Extract the shaka.util.Error object from the event.
      this.onError(event.detail);
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
        self.timestamp = xhr.responseText.trim();
        console.log(self.timestamp);
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