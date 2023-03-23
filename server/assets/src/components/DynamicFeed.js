import React, { Component, useState } from "react"
import { StylesProvider, ThemeProvider } from "@material-ui/styles"
import { Paper, Box, Typography, makeStyles, Grid } from "@material-ui/core"

import theme from "./theme"

import SiteMenu from "./SiteMenu"
import Player from "./Player"

import "styles/home.scss"

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

const feedFromSlug = feedSlug => ({
  name: feedSlug,
  slug: feedSlug,
  nodeName: feedSlug
})

const DynamicFeed = props => {
  const classes = useStyles()
  const { feedSlug } = props.match.params
  const feed = feedFromSlug(feedSlug)

  return (
    <>
      <StylesProvider injectFirst>
        <ThemeProvider theme={theme}>
          <Paper
            square
            elevation={0}
            component="div"
            style={{ position: "relative" }}
          >
            <SiteMenu />
            <Grid
              container
              direction="column"
              justify="center"
              alignItems="center"
            >
              <Grid item>
                <Paper elevation={0}>
                  <Typography variant="h5" component="h5">
                    <Box
                      mt={{ xs: 2, sm: 4 }}
                      mr={{ xs: 3, sm: 10, md: 12 }}
                      mb={{ xs: 2, sm: 4 }}
                      ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
                    >
                      {feedSlug} test page
                    </Box>
                  </Typography>

                  <Player currentFeed={feed} />
                </Paper>
              </Grid>
            </Grid>
          </Paper>
        </ThemeProvider>
      </StylesProvider>
    </>
  )
}

export default DynamicFeed
