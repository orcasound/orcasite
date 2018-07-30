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
    document.title = 'Orcasound Beta'
  }

  changeFeed = currentFeed => this.setState({currentFeed})

  render() {
    const {feedSlug} = this.props.match.params

    return (
      <div className="home">
        <Header />
        <FeedList />
        <div className="content">
          {feedSlug && (
            <FeedPage feedSlug={feedSlug} onChangeFeed={this.changeFeed} />
          )}
          <p>
            Please help us test playback performance on as many combinations of
            devices, operating systems, and browsers as possible.
          </p>
          <p>
            <a href="https://goo.gl/forms/hLbgpnR2w9bmWt902">
              Provide your feedback via this Google form
            </a>.
          </p>
        </div>

        <Player currentFeed={this.state.currentFeed} />
      </div>
    )
  }

}
