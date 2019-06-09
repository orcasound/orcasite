import React, { Component } from 'react'
import { Query } from 'react-apollo'
import { string, func } from 'prop-types'
import { GET_FEED } from '../queries/feeds'
import Loader from './Loader'
import { Paper, Typography } from '@material-ui/core'
import styled from "styled-components"

const FeedName = styled(Typography)`
  font-size: 20px;
  font-weight: 400;
  color: rgba(0,0,0,0.87);
  letter-spacing: 0;
  text-align: left;
  line-height: 28px;
`

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

          // let lat, long
          // if (feed.locationPoint && feed.locationPoint.coordinates) {
          //   const {
          //     locationPoint: { coordinates: [lat, long] = [] },
          //   } = feed
          // }

          const { introHtml, thumbUrl } = feed

          return (
            <Paper>
              <FeedName variant="h2">{feed.name}</FeedName>
              <img
                src="https://via.placeholder.com/375x127"
              />
              {introHtml && <div dangerouslySetInnerHTML={{ __html: introHtml }} />}

            </Paper>
          )
        }}
      </Query>
    )
  }
}

export default FeedPageV2
