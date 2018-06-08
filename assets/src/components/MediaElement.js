import React, { Component } from 'react';
import flvjs from 'flv.js';
import hlsjs from 'hls.js';
import 'mediaelement';

// Import stylesheet and shims
import 'mediaelement/build/mediaelementplayer.min.css';
import 'mediaelement/build/mediaelement-flash-video.swf';

export default class MediaElement extends Component {

  state = {}

  success(media, node, instance) {
    // Your action when media was successfully loaded
  }

  error(media) {
    // Your action when media had an error loading
  }

  render() {

    const
      props = this.props,
      sources = JSON.parse(props.sources),
      tracks = JSON.parse(props.tracks),
      sourceTags = [],
      tracksTags = []
    ;

    for (let i = 0, total = sources.length; i < total; i++) {
      const source = sources[i];
      sourceTags.push(`<source src="${source.src}" type="${source.type}">`);
    }

    for (let i = 0, total = tracks.length; i < total; i++) {
      const track = tracks[i];
      tracksTags.push(`<track src="${track.src}" kind="${track.kind}" srclang="${track.lang}"${(track.label ? ` label=${track.label}` : '')}>`);
    }

    const
      mediaBody = `${sourceTags.join("\n")}
        ${tracksTags.join("\n")}`,
      mediaHtml = props.mediaType === 'video' ?
        `<video id="${props.id}" width="${props.width}" height="${props.height}"${(props.poster ? ` poster=${props.poster}` : '')}
          ${(props.controls ? ' controls' : '')}${(props.preload ? ` preload="${props.preload}"` : '')}>
          ${mediaBody}
        </video>` :
        `<audio id="${props.id}" width="${props.width}" controls>
          ${mediaBody}
        </audio>`
    ;

    return (<div dangerouslySetInnerHTML={{__html: mediaHtml}}></div>);

  }

  componentDidMount() {

    const {MediaElementPlayer} = global;
    
    if (!MediaElementPlayer) {
      return;
    }

    const options = Object.assign({}, JSON.parse(this.props.options), {
      // Read the Notes below for more explanation about how to set up the path for shims
      pluginPath: './static/media/',
      success: (media, node, instance) => this.success(media, node, instance),
      error: (media, node) => this.error(media, node)
    });
    
    window.flvjs = flvjs;
    window.Hls = hlsjs;
    this.setState({player: new MediaElementPlayer(this.props.id, options)});
  }

  componentWillUnmount() {
    if (this.state.player) {
      this.state.player.remove();
      this.setState({player: null});
    }
  }
}