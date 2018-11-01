import React, {Component} from 'react'

import {gql} from 'apollo-boost'
import {Query} from 'react-apollo'

import Player from './Player'
import Header from './Header'
import FeedList from './FeedList'
import FeedPage from './FeedPage'

import 'styles/home.scss'

export default class Home extends Component {
  state = {}

  componentDidMount() {
    if (['beta', 'dev', 'staging'].indexOf(ENV.ENV_NAME) >= 0) {
      document.title = `Orcasound ${ENV.ENV_NAME}`
    } else {
      document.title = `Orcasound`
    }
  }

  changeFeed = currentFeed => this.setState({currentFeed, autoplay: true})

  render() {
    const {feedSlug} = this.props.match.params

    return (
      <div className="home">
        <Header />
        <FeedList />
        <div className="content container">
          {feedSlug && (
            <FeedPage feedSlug={feedSlug} onChangeFeed={this.changeFeed} />
          )}
          {!feedSlug && (
            <div className="home-content">
              <h1 className="my-4">Orcasound</h1>
              <p>
                Listen to live underwater sound in orca habitat! Choose a
                location on the left (or above on mobile), then select the green
                listen button. The control bar on the bottom lets you pause/play
                and see how many are listening for whales with you.
              </p>
              <p>
                While you listen, you can also{' '}
                <a href="http://www.orcasound.net/learn/">
                  learn more about orca sounds
                </a>.
              </p>
              <p>
                Launched in November, 2018, the Orcasound app makes it easy for
                everyone to listen for whales. We welcome feedback about your
                listening experience via this{' '}
                <a href="https://goo.gl/forms/tgi4zoEDOFf5zQRJ3">
                  user experience survey
                </a>. If you have trouble, try reloading the site after clearing
                your browser's cache.
              </p>
            </div>
          )}
        </div>

        <Player
          currentFeed={this.state.currentFeed}
          key={this.state.currentFeed && this.state.currentFeed.nodeName}
          autoplay={this.state.autoplay}
        />
      </div>
    )
  }
}
