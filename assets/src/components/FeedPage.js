import React, { Component } from "react"
import { Query } from "react-apollo"
import { string, func } from "prop-types"
import { Typography } from "@material-ui/core"
import styled from "styled-components"

import { GET_FEED } from "../queries/feeds"

import Loader from "./Loader"
import FeedPresence from "./FeedPresence"

const FeedPageContainer = styled.div`
  img {
    z-index: 0;
  }
`
const FeedName = styled(Typography)`
  font-size: 20px;
  font-weight: 400;
  color: rgba(0, 0, 0, 0.87);
  letter-spacing: 0;
  text-align: left;
  line-height: 28px;
  padding: 1rem 0rem 1rem 2rem;
`

const FeedImageContainer = styled.div`
  background-repeat: no-repeat;
  -webkit-background-size: cover;
  -moz-background-size: cover;
  -o-background-size: cover;
  background-size: cover;
  background-position: center;
  background-color: gray;
  height: 127px;
`

const StyledIntroHTML = styled.div`
  padding: 1rem 3rem 1rem 2rem;
`

class FeedPage extends Component {
  state = { show: false }

  static propTypes = {
    feedSlug: string.isRequired,
    onChangeFeed: func
  }

  showModal = () => {
    this.setState({ show: true })
  }

  hideModal = () => {
    this.setState({ show: false })
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

          const { introHtml, thumbUrl, mapUrl } = feed

          return (
            <FeedPageContainer>
              <FeedName variant="h2">{feed.name}</FeedName>
              <FeedImageContainer
                style={{
                  backgroundImage: `url(${mapUrl})`
                }}
              ></FeedImageContainer>
              {this.props.children}
              <FeedPresence feed={feed} />
              <StyledIntroHTML>
                {introHtml && (
                  <div dangerouslySetInnerHTML={{ __html: introHtml }} />
                )}
              </StyledIntroHTML>
            </FeedPageContainer>
          )
        }}
      </Query>
    )
  }
}

export default FeedPage
