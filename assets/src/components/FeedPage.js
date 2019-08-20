import React from "react"
import { Query } from "react-apollo"
import { string, func } from "prop-types"
import { GET_FEED } from "../queries/feeds"
import Loader from "./Loader"
import { Paper, Box, Typography } from "@material-ui/core"
import styled from "styled-components"

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

const FeedPage = props => {
  const { feedSlug: slug } = props

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
          <Paper elevation={0}>
            <Typography variant="h5" component="h5">
              <Box
                mt={{ xs: 2, sm: 4 }}
                mr={{ xs: 3, sm: 10, md: 12 }}
                mb={{ xs: 2, sm: 4 }}
                ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
              >
                {feed.name}
              </Box>
            </Typography>

            <FeedImageContainer
              style={{
                backgroundImage: `url(${mapUrl})`
              }}
            />
            {props.children}
            <Paper elevation={0}>
              {introHtml && (
                <Typography variant="body1" component="div">
                  <Box
                    mt={{ xs: 2, sm: 4, md: 6 }}
                    mr={{ xs: 6, sm: 30, md: 50, lg: 60 }}
                    ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
                    fontSize={{ xs: "0.875rem", sm: "1rem" }}
                  >
                    <Paper
                      variant="body1"
                      component="div"
                      elevation={0}
                      dangerouslySetInnerHTML={{ __html: introHtml }}
                    />
                  </Box>
                </Typography>
              )}
            </Paper>
          </Paper>
        )
      }}
    </Query>
  )
}

FeedPage.propTypes = {
  feedSlug: string.isRequired,
  onChangeFeed: func
}

export default FeedPage
