import React, {Component} from 'react'

export default class FeedPage extends Component {
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
