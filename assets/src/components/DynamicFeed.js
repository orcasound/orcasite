import React, {Component} from 'react'

import {gql} from 'apollo-boost'
import {Query} from 'react-apollo'

import Player from './Player'
import Header from './Header'
import FeedList from './FeedList'

import 'styles/home.scss'

export default class DynamicFeed extends Component {
  state = {}

  componentDidMount() {
    document.title = 'Orcasound Beta'
  }

  changeFeed = currentFeed => this.setState({currentFeed})
  feedFromSlug = feedSlug => ({
    name: feedSlug,
    slug: feedSlug,
    nodeName: feedSlug,
  })

  render() {
    const {feedSlug} = this.props.match.params

    return (
      <div className="home">
        <Header />
        <FeedList />
        <div className="content">
          <h1>This is a testing page for {feedSlug}.</h1>
          <button
            className="btn btn-primary"
            onClick={() => this.changeFeed(this.feedFromSlug(feedSlug))}>
            Listen to {feedSlug}
          </button>
        </div>

        <Player
          currentFeed={this.state.currentFeed}
          key={this.state.currentFeed && this.state.currentFeed.nodeName}
        />
      </div>
    )
  }
}
