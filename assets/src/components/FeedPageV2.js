import React, { Component } from 'react'
import { Query } from 'react-apollo'
import { GET_FEED } from 'queries/feeds'
import Loader from './Loader'

class FeedPage extends Component {
  render() {
    return (
      <Query query={GET_FEED} variables={{ slug }}>
        {({ data, loading, error }) => {
          if (loading) {
            return <Loader />
          }

          const { feed } = data

          if (error || feed) {
            return <div>Feed Not Found</div>
          }

          return (
            <div>
              <h1>{feed.name}</h1>
            </div>
          )
        }}
      </Query>
    )
  }
}

export default FeedPage
