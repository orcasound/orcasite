import React, { useState } from "react"
import { Link } from "react-router-dom"
import { Query } from "react-apollo"
import {
  Grid,
  Button,
  ClickAwayListener,
  Grow,
  Paper,
  Popper,
  MenuList,
  MenuItem,
  Box
} from "@material-ui/core"
import { ArrowDropDown, Person } from "@material-ui/icons"
import styled from "styled-components"

import { LIST_FEEDS } from "../queries/feeds"

const FeedListGridContainer = styled.div`
  display: flex;
  flex-grow: 1;
  max-width: none;
  flex-shrink: 1;
`

const FeedButton = styled(Button)`
  display: flex;
  flex-grow: 1;
  flex-shrink: 1;
  padding: 0.375rem 0rem 0.375rem 0rem;
  height: 3rem;
`

const FeedMenuItem = styled(MenuItem)`
  display: flex;
  justify-content: space-between;

  a {
    color: rgba(0, 0, 0, 0.87);
  }

  color: #000000;
`

const FeedList = React.forwardRef((props, ref) => {
  //const [open, setToggle] = useState(true)
  let [anchorEl, setAnchorEl] = useState(null)

  const handleClick = e => {
    setAnchorEl(anchorEl ? null : e.currentTarget)
  }

  const open = Boolean(anchorEl)
  const id = open ? "simple-popper" : undefined

  const { bushPointUsers, haroStraitUsers } = props

  return (
    <Query query={LIST_FEEDS}>
      {({ data, loading, error }) => {
        if (loading || error) {
          return (
            <ul>
              <li>
                <div>LOADING</div>
              </li>
            </ul>
          )
        }

        const { feeds } = data
        return (
          <FeedListGridContainer>
            <FeedButton
              aria-describedby={id}
              variant="text"
              onClick={handleClick}
            >
              <Box letterSpacing="0.03572em">Listen Live</Box>
              <ArrowDropDown />
            </FeedButton>

            <Popper
              id={id}
              open={open}
              anchorEl={anchorEl}
              keepMounted
              transition
            >
              {({ TransitionProps, placement }) => (
                <Grow
                  {...TransitionProps}
                  id="menu-list-grow"
                  style={{
                    transformOrigin:
                      placement === "bottom" ? "center top" : "center bottom"
                  }}
                >
                  <Paper>
                    <MenuList>
                      {feeds
                        .slice()
                        .reverse()
                        .map((feed, i) => {
                          return (
                            <FeedMenuItem key={i}>
                              <Link to={`/${feed.slug}`}>
                                <span>{feed.name}</span>
                                <div>
                                  {bushPointUsers}
                                  <Person />
                                </div>
                              </Link>
                            </FeedMenuItem>
                          )
                        })}
                    </MenuList>
                  </Paper>
                </Grow>
              )}
            </Popper>
          </FeedListGridContainer>
        )
      }}
    </Query>
  )
})

FeedList.defaultProps = {
  bushPointUsers: 1,
  haroStraitUsers: 3
}

export default FeedList
