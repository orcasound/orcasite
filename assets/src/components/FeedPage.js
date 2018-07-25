import React, {Component} from 'react'

import {string, func} from 'prop-types'

export default class FeedPage extends Component {
  static propTypes = {
    nodeName: string,
    onChangeFeed: func

  }
  render() {
    return (
      <div className="feed-page mb-3">
        <h1>{this.props.nodeName} node</h1>

        <button className="btn btn-primary" onClick={() => this.props.onChangeFeed(this.props.nodeName)}>
          Listen to {this.props.nodeName}
        </button>
      </div>
    )
  }
}
