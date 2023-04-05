import React, { Component } from "react"
import socket from "../utils/socket"
import { Presence } from "phoenix"

import { feedType } from "../types/feedType"

import { FontAwesomeIcon } from "@fortawesome/react-fontawesome"
import { faUser } from "@fortawesome/free-solid-svg-icons"

export default class FeedPresence extends Component {
  static propTypes = {
    feed: feedType.isRequired
  }

  state = {
    listenerCount: 0
  }

  componentDidMount() {
    this.joinFeedChannel(this.props.feed.id)
  }

  componentDidUpdate(prevProps) {
    if (this.props.feed.id !== prevProps.feed.id) {
      this.joinFeedChannel(this.props.feed.id)
    }
  }

  joinFeedChannel(feed_id) {
    if (this.state.channel) {
      this.state.channel.leave()
    }

    this.setState({ channel: socket.channel(`feed:${feed_id}`, {}) }, () => {
      let channel = this.state.channel
      let presence = new Presence(channel)
      presence.onSync(() => this.setListeners(presence))
      channel.join()
    })
  }

  componentWillUnmount() {
    if (this.state.channel) {
      this.state.channel.leave()
    }
  }

  setListeners({ state }) {
    const { id } = this.props.feed
    const listenerCount = state[id] ? state[id].metas.length : 0
    this.setState({ listenerCount })
    this.props.onListenerChange && this.props.onListenerChange(listenerCount)
  }

  render() {
    const { listenerCount } = this.state

    return (
      <div className={`feed-presence text-nowrap ${this.props.className}`}>
        {listenerCount > 0 && listenerCount}
        {listenerCount > 0 && (
          <FontAwesomeIcon icon={faUser} className="ml-2" />
        )}
      </div>
    )
  }
}
