import React, { Component } from "react"

import { gql } from "apollo-boost"
import { Query } from "react-apollo"

import Player from "./Player"
import Header from "./Header"
import FeedList from "./FeedList"
import FeedPage from "./FeedPage"

import "styles/home.scss"

export default class Home extends Component {
  state = {}

  componentDidMount() {
    if (["beta", "dev", "staging"].indexOf(ENV.ENV_NAME) >= 0) {
      document.title = `Orcasound ${ENV.ENV_NAME}`
    } else {
      document.title = `Orcasound`
    }
  }

  changeFeed = currentFeed => this.setState({ currentFeed, autoplay: true })

  render() {
    const { feedSlug } = this.props.match.params

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
                Listen to live underwater sound in orca habitat! If you're lucky
                you'll hear orcas, but mostly you'll hear ships...
              </p>
              <p>
                Choose a location and then select the green listen button. The
                control bar at the bottom lets you pause/play and shows how many
                people are listening for whales with you.{" "}
              </p>
              <p>this is a test</p>
              <p>
                While you listen, you can also{" "}
                <a href="http://www.orcasound.net/learn/" target="_blank">
                  learn more about orca sounds
                </a>
                . If you need some more inspiration, here is five minutes of J &
                K pod orcas calling, whistling, and clicking:
              </p>

              <audio controls>
                <source
                  src="http://www.orcasound.net/data/product/SRKW/greatest-hits/orcasite-4min-sample.ogg"
                  type="audio/ogg"
                />
                <source
                  src="http://www.orcasound.net/data/product/SRKW/greatest-hits/orcasite-4min-sample.mp3"
                  type="audio/mpeg"
                />
              </audio>
              <p>
                Launched in November, 2018, the Orcasound app makes it easy for
                everyone to listen for whales. We welcome feedback about your
                listening experience via this{" "}
                <a
                  href="https://goo.gl/forms/tgi4zoEDOFf5zQRJ3"
                  target="_blank"
                >
                  user experience survey
                </a>
                . If you have trouble, try reloading the site after clearing
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
