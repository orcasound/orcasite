import React, {Component} from 'react'
import {Query} from 'react-apollo'

import {string, func} from 'prop-types'

import {GET_FEED} from 'queries/feeds'

import Loader from 'components/Loader'

export default class FeedPage extends Component {
  static propTypes = {
    feedSlug: string.isRequired,
    onChangeFeed: func,
  }
  render() {
    const {feedSlug: slug} = this.props
    return (
      <Query query={GET_FEED} variables={{slug}}>
        {({data, loading, error}) => {
          if (loading) {
            return <Loader />
          }

          if (error) {
            return <div>Couldn't find feed for {slug}</div>
          }

          const {feed} = data
          const [lat, long] = feed.locationPoint.coordinates
          return (
            <div className="feed-page mb-3">
              <h1>{feed.name} node</h1>
              <p>Located at: {lat}, {long}</p>

              <button
                className="btn btn-primary"
                onClick={() => this.props.onChangeFeed(feed)}>
                Listen to {feed.name}
              </button>
            </div>
          )
        }}
      </Query>
    )
  }
}
