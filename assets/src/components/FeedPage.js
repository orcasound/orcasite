import React, { useState } from "react"
import { Paper, Box, Typography, makeStyles } from "@material-ui/core"
import { Query } from "react-apollo"
import { string, func } from "prop-types"

import { GET_FEED } from "../queries/feeds"
import Player from "./Player"
import FeedPresence from "./FeedPresence"
import Loader from "./Loader"

const useStyles = makeStyles(theme => ({
  paper: {
    [theme.breakpoints.down("xs")]: {
      height: "8rem"
    },
    [theme.breakpoints.up("sm")]: {
      height: "10rem"
    },
    [theme.breakpoints.up("md")]: {
      height: "12rem"
    },
    [theme.breakpoints.up("lg")]: {
      height: "14rem"
    },
    [theme.breakpoints.up("xl")]: {
      height: "16rem"
    },
    backgroundRepeat: "no-repeat",
    backgroundSize: "cover",
    backgroundPosition: "center",
    backgroundColor: "gray"
  }
}))

const FeedPage = props => {
  const classes = useStyles()
  const { feedSlug: slug } = props

  const [listenerCount, setListenerCount] = useState(0)

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

            <Paper
              className={classes.paper}
              style={{
                backgroundImage: `url(${mapUrl})`
              }}
            />

            <Player currentFeed={feed} listenerCount={listenerCount} />

            <Paper elevation={0}>
              <FeedPresence
                feed={feed}
                className="text-center"
                onListenerChange={setListenerCount}
              />
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
