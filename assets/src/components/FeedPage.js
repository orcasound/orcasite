import React, { Component } from 'react'
import { Query } from 'react-apollo'

import { string, func } from 'prop-types'

import { GET_FEED } from 'queries/feeds'

import Loader from 'components/Loader'

export default class FeedPage extends Component {
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
            return <h4 className="mb-4">Couldn't find feed for {slug}</h4>
          }

          let lat, long
          if (feed.locationPoint && feed.locationPoint.coordinates) {
            const {
              locationPoint: { coordinates: [lat, long] = [] },
            } = feed
          }

          const { introHtml } = feed

          return (
            <div className="feed-page mb-4">
              <h1 className="my-4">{feed.name}</h1>
              {lat &&
                long && (
                  <p>
                    Located at: {lat}, {long}
                  </p>
                )}

              <button
                className="btn btn-primary mb-3"
                onClick={() => this.props.onChangeFeed(feed)}>
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
