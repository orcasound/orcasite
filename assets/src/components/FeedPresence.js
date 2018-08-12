import React, {Component} from 'react'
import socket from 'utils/socket'
import {Presence} from 'phoenix'

import {feedType} from 'types/feedType'

import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faUser} from '@fortawesome/free-solid-svg-icons'

export default class FeedPresence extends Component {
  static propTypes = {
    feed: feedType.isRequired,
  }

  state = {
    presences: {},
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

    this.setState({channel: socket.channel(`feed:${feed_id}`, {})}, () => {
      var presences = {}
      let channel = this.state.channel
      channel.on('presence_state', state => {
        presences = Presence.syncState(presences, state)
        this.setListeners(presences)
      })

      channel.on('presence_diff', diff => {
        presences = Presence.syncDiff(presences, diff)
        this.setListeners(presences)
      })

      channel.join()
    })
  }

  componentWillUnmount() {
    if (this.state.channel) {
      this.state.channel.leave()
    }
  }

  setListeners(presences) {
    const {id} = this.props.feed
    const listenerCount = presences[id] ? presences[id].metas.length : 0
    this.setState({listenerCount})
  }

  render() {
    const {listenerCount} = this.state
    return (
      <div className={`feed-presence text-nowrap ${this.props.className}`}>
        {listenerCount}
        {listenerCount && <FontAwesomeIcon icon={faUser} className="ml-2" />}
      </div>
    )
  }
}
