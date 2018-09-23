window.AudioContext = window.AudioContext || window.webkitAudioContext;
navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia;

import React, {Component} from 'react'

export default class Spectrogram extends Component {
  constructor(props) {
    super(props)
  }
  componentDidMount() {

    var self = this;
    self.canvas = document.getElementById(this.props.componentId);
    self.width  = self.canvas.width;
    self.height = self.canvas.height;
    self.ctx = self.canvas.getContext('2d');
    self.imageData = self.ctx.getImageData(0, 0, self.width, self.height);

    self.data = self.imageData.data;

    self.buf = new ArrayBuffer(self.imageData.data.length);
    self.buf8 = new Uint8ClampedArray(self.buf);
    self.data32 = new Uint32Array(self.buf);

    self.x = 0;
    self.bufferSize = 1024;
    self.dataBuffer = new Float32Array(self.height);
    self.uint8array = new Uint8Array(4);
    self.colorData = new Uint8Array(4 * self.bufferSize / 2);
    this.interval = setInterval(() => this.drawFrame(), 20)

    //this.gotStream()
    //this.audio = new Audio()
    //this.audio.src = this.props.src
    //console.log("audio")
    //console.log(this.props)
    //console.log(this.audio)
    //document.body.appendChild(this.audio)
    navigator.getUserMedia({audio: true}, this.gotStream, function() { console.log("error"); });
    //this.gotStream()
  }

  componentWillUnmount() {
    clearInterval(this.interval)
  }

  gotStream = (stream) => {
      var self = this;
      self.context = new window.AudioContext();
      self.sampleRate = self.context.sampleRate;

      var input = self.context.createMediaStreamSource(stream);

      //var input = self.context.createMediaElementSource(this.audio)
      //var input = self.context.createMediaElementSource(this.props.audio.captureStream())
      self.analyser = self.context.createAnalyser();
      self.analyser.fftSize = self.bufferSize;
      input.connect(self.analyser);
  }

  setPixel = (x, y, red, green, blue, alpha) => {
     var self = this;
      self.data32[y * self.width + x] =
          (alpha << 24) |    // alpha
          (blue << 16) |      // blue
          (green <<  8) |     // green
          red;                // red
  }

  getPixel = (x, y) => {
    var self = this;
      var value = self.data32[y * self.width + x];
      var channels = self.uint32Touint8(value)
      return channels;
  }

  uint32Touint8 = (uint32) => {
    var self = this;
      self.uint8array[3] = uint32 >> 24 & 0xff;
      self.uint8array[2] = uint32 >> 16 & 0xff;
      self.uint8array[1] = uint32 >> 8 & 0xff;
      self.uint8array[0] = uint32 & 0xff;
      return data
  }

  drawColumn = () => {
    var self = this;
      var value = 0;
      for (var y = 0; y < self.height; y++) {
          var x = col;
          self.setPixel(x, y, value, value, value, 255)
      }
  }

  getData = () => {
    var self = this;
      var freqByteData = new Uint8Array(self.analyser.frequencyBinCount);
      self.analyser.getByteFrequencyData(freqByteData);

      // Reverse the direction, making lower frequencies on the bottom.
      for (var i = self.analyser.frequencyBinCount - 1; i >= 0; i--) {
          self.dataBuffer[i] = freqByteData[(self.analyser.frequencyBinCount - 1) - i] / 255.0;
      }

      return self.dataBuffer
  }

  color = (value) => {
    var self = this;
      var rgb = {R: 0, G: 0, B: 0};

      if (0 <= value && value <= 1 / 8) {
          rgb.R = 0;
          rgb.G = 0;
          rgb.B = 4 * value + .5; // .5 - 1 // b = 1/2
      } else if (1 / 8 < value && value <= 3 / 8) {
          rgb.R = 0;
          rgb.G = 4 * value - .5; // 0 - 1 // b = - 1/2
          rgb.B = 0;
      } else if (3 / 8 < value && value <= 5 / 8) {
          rgb.R = 4*value - 1.5; // 0 - 1 // b = - 3/2
          rgb.G = 1;
          rgb.B = -4 * value + 2.5; // 1 - 0 // b = 5/2
      } else if (5 / 8 < value && value <= 7 / 8) {
          rgb.R = 1;
          rgb.G = -4 * value + 3.5; // 1 - 0 // b = 7/2
          rgb.B = 0;
      } else if (7 / 8 < value && value <= 1) {
          rgb.R = -4*value + 4.5; // 1 - .5 // b = 9/2
          rgb.G = 0;
          rgb.B = 0;
      } else {    // should never happen - value > 1
          rgb.R = .5;
          rgb.G = 0;
          rgb.B = 0;
      }

      return [rgb.R, rgb.G, rgb.B, 1].map(function(d) { return parseInt(d * 255, 10)})
  }

  colorizeData = (data) => {
    var self = this;
      var d;
      for(var i = 0, n = data.length; i < n; i++) {
          d = self.color(data[i]);
          self.colorData.set(d, i * 4);
      }
      return self.colorData;
  }

  addColumn = (colorizeData) => {
    var self = this;
      for (var y = 0; y < self.height; y++) {
          self.setPixel(self.x, y, colorizeData[4 * y + 0], colorizeData[4 * y + 1], colorizeData[4 * y + 2], colorizeData[4 * y + 3]);
      }
      self.x++;
      self.x %= self.width;
  }

  drawFrame = (data) => {
    var self = this;
      var data = data || self.getData();
      var colorData = self.colorizeData(data)

      self.addColumn(colorData);
      self.imageData.data.set(self.buf8);
      self.ctx.putImageData(self.imageData, 0, 0);
  }

  render() {
    const {src} = this.props
    return (
      <a>
      <canvas id='viewport' height="512" width="700"/>
      </a>)
  }
}
