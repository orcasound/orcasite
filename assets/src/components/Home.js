import React, {Component} from 'react'

import {gql} from 'apollo-boost'
import {Query} from 'react-apollo'

import {FeedContext} from 'contexts/feed-context'

import AudioPlayer from './AudioPlayer'
import Header from './Header'

import 'styles/home.scss'

export default class Home extends Component {
  static defaultProps = {
    nodeName: 'rpi_seattle',
  }

  constructor(props) {
    super(props)

    this.state = {
      nodeName: props.match.params.nodeName || props.nodeName,
    }
  }

  componentDidMount() {
    document.title = 'Orcasound Beta'
  }

  render() {
    var {nodeName} = this.state

    return (
      <div className="home">
        <Header />
        <div className="content">
          <p>
            Please help us test playback performance on as many combinations
            of devices, operating systems, and browsers as possible.
          </p>
          <p>
            <a href="https://goo.gl/forms/hLbgpnR2w9bmWt902">
              Provide your feedback via this Google form
            </a>.
          </p>
        </div>

        <FeedContext.Consumer>
          {currentFeed => (
            <AudioPlayer currentFeed={currentFeed} />
          )}
        </FeedContext.Consumer>
      </div>
    )
  }
}
