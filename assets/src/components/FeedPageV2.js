import React, { Component } from 'react'
import { Query } from 'react-apollo'
import { string, func } from 'prop-types'
import { GET_FEED } from '../queries/feeds'
import Loader from './Loader'

class FeedPageV2 extends Component {
  static propTypes = {
    feedSlug: string.isRequired,
    onChangeFeed: func,
  }

  render() {
    const { feedSlug: slug } = this.props

    return (
      <Query query={GET_FEED} variables={{ slug }}>
        {({ data, loading, error }) => {
          if (loading) {
            return <Loader />
          }

          const { feed } = data

          if (error || !feed) {
            return <div>Feed Not Found for {slug}</div>
          }

          let lat, long
          if (feed.locationPoint && feed.locationPoint.coordinates) {
            const {
              locationPoint: { coordinates: [lat, long] = [] },
            } = feed
          }

          const { introHtml } = feed

          return (
            <div>
              <h1>{feed.name}</h1>
              {lat &&
                long && (
                  <p>Located at: {lat}, {long}</p>
                )
              }
              <button onClick={() => this.props.onChangeFeed(feed)}>
                Listen to {feed.name}
              </button>

              {introHtml && <div dangerouslySetInnerHTML={{ __html: introHtml }} />}

            </div>
          )
        }}
      </Query>
    )
  }
}

export default FeedPageV2
