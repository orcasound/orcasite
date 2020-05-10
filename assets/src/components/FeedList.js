import React, { useState } from "react"
import { Link } from "react-router-dom"
import { Query } from "react-apollo"
import {
  Button,
  ClickAwayListener,
  Grow,
  Paper,
  Popper,
  MenuList,
  MenuItem,
  Box,
  Typography,
  Tooltip,
  makeStyles,
  Hidden,
} from "@material-ui/core"
import { ArrowDropDown, Person, GraphicEq } from "@material-ui/icons"
import FeedPresence from "./FeedPresence"

import { feedType } from "types/feedType"
import { storeCurrentFeed, getCurrentFeed } from "utils/feedStorage"
import { LIST_FEEDS } from "../queries/feeds"

const useStyles = makeStyles((theme) => ({
  paper: {
    zIndex: 99,
    // position: "relative",
  },
  button: {
    height: "3rem",
    zIndex: 1,
  },
  mobileButton: {
    height: "3rem",
    zIndex: 1,
    padding: 0,
  },
  menuList: {
    right: "8rem",
  },
  menuItem: {
    // display: "flex",
    // justifyContent: "space-between",
    color: theme.palette.common.black,
    "& a": {
      color: theme.palette.common.black,
      "&:hover": {
        textDecoration: "none",
        color: "#000000",
      },
      "&:active": {
        color: "#15766b",
      },
      "&:visited": {
        color: "#000000",
      },
    },
  },
}))

const FeedList = React.forwardRef((props, ref) => {
  const [open, setOpen] = useState(false)
  const anchorRef = React.useRef(null)
  const classes = useStyles()

  const handleToggle = () => {
    setOpen((prevOpen) => !prevOpen)
  }

  const handleClose = (event) => {
    if (anchorRef.current && anchorRef.current.contains(event.target)) {
      return
    }

    setOpen(false)
  }

  return (
    <Query query={LIST_FEEDS}>
      {({ data, loading, error }) => {
        const { feeds } = data

        return (
          <ClickAwayListener onClickAway={handleClose}>
            <div>
              {/** --- MOBILE VIEW --- */}
              <Hidden smUp>
                <Button
                  className={classes.mobileButton}
                  ref={anchorRef}
                  aria-controls="menu-list-grow"
                  aria-haspopup="true"
                  variant="contained"
                  onClick={handleToggle}
                  color="primary"
                >
                  <GraphicEq />
                  <Hidden xsDown>
                    <Box letterSpacing="0.03572em">Listen Live</Box>
                  </Hidden>
                  <Tooltip arrow title={<Typography>Listen Live</Typography>}>
                    <ArrowDropDown />
                  </Tooltip>
                </Button>
              </Hidden>
              {/** --- TABLET -> DESKTOP BREAKPOINT --- */}
              <Hidden xsDown>
                <Button
                  className={classes.button}
                  ref={anchorRef}
                  aria-controls="menu-list-grow"
                  aria-haspopup="true"
                  variant="contained"
                  onClick={handleToggle}
                  color="primary"
                >
                  <GraphicEq />
                  <Hidden xsDown>
                    <Box letterSpacing="0.03572em">Listen Live</Box>
                  </Hidden>
                  <Tooltip arrow title={<Typography>Listen Live</Typography>}>
                    <ArrowDropDown />
                  </Tooltip>
                </Button>
              </Hidden>
              <Popper
                open={open}
                anchorEl={anchorRef.current}
                keepMounted
                transition
              >
                {({ TransitionProps, placement }) => (
                  <Grow
                    {...TransitionProps}
                    style={{
                      transformOrigin:
                        placement === "bottom" ? "center top" : "center bottom",
                    }}
                  >
                    <Paper className={classes.paper}>
                      {loading ? (
                        <Paper>Loading</Paper>
                      ) : (
                        <MenuList className={classes.MenuList}>
                          {feeds
                            .slice()
                            .reverse()
                            .map((feed, i) => {
                              return (
                                <MenuItem
                                  className={classes.menuItem}
                                  key={feed.name}
                                  component={Link}
                                  to={`/${feed.slug}`}
                                >
                                  {feed.name}
                                </MenuItem>
                              )
                            })}
                        </MenuList>
                      )}
                    </Paper>
                  </Grow>
                )}
              </Popper>
            </div>
          </ClickAwayListener>
        )
      }}
    </Query>
  )
})

FeedList.propTypes = {
  feed: feedType,
}

export default FeedList
