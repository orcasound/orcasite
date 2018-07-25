import React, {Component} from 'react'

import {func} from 'prop-types'

import {feedType} from 'types/feedType'

export default class FeedPage extends Component {
  static propTypes = {
    feed: feedType,
    onChangeFeed: func
  }
  render() {
    const {feed} = this.props
    return (
      <div className="feed-page mb-3">
        <h1>{feed.name} node</h1>

        <button className="btn btn-primary" onClick={() => this.props.onChangeFeed(feed)}>
          Listen to {feed.name}
        </button>
      </div>
    )
  }
}
