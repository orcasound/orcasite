import React, { Component } from "react"
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

const FeedListGridContainer = styled(Grid)`
  flex-grow: 1;
  max-width: none;
  flex-shrink: 1;
`

const FeedButton = styled(Button)`
  display: flex;
  flex-grow: 1;
  flex-shrink: 1;
  padding: 0.375rem 0rem 0.375rem 0rem;
`

const FeedMenuItem = styled(MenuItem)`
  display: flex;
  justify-content: space-between;

  a {
    color: rgba(0, 0, 0, 0.87);
  }

  color: #000000;
`

// -------------------------------------------
// Component
// -------------------------------------------
class FeedList extends Component {
  constructor(props) {
    super(props)

    this.handleToggle = this.handleToggle.bind(this)
  }

  state = {
    open: false,
    bushPointUsers: 1,
    haroStraitUsers: 3
  }

  handleToggle() {
    this.setState(state => ({ open: !state.open }))
  }

  handleClose = event => {
    if (this.anchorEl.contains(event.target)) {
      return
    }

    this.setState({ open: false })
  }

  render() {
    const { open, bushPointUsers, haroStraitUsers } = this.state

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
            <FeedListGridContainer
              container
              direction="column"
              justify="flex-start"
              alignItems="center"
              alignContent="center"
            >
              <Grid item>
                <FeedButton
                  variant="text"
                  buttonRef={node => {
                    this.anchorEl = node
                  }}
                  aria-owns={open ? "menu-list-grow" : undefined}
                  aria-haspopup="true"
                  onClick={this.handleToggle}
                >
                  <Box letterSpacing="0.03572em">Listen Live</Box>
                  <ArrowDropDown />
                </FeedButton>
              </Grid>

              <Popper
                open={open}
                anchorEl={this.anchorEl}
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
                      <ClickAwayListener onClickAway={this.handleClose}>
                        <MenuList>
                          {feeds
                            .slice()
                            .reverse()
                            .map((feed, i) => {
                              return (
                                <FeedMenuItem
                                  key={i}
                                  onClick={this.handleClose}
                                >
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
                      </ClickAwayListener>
                    </Paper>
                  </Grow>
                )}
              </Popper>
            </FeedListGridContainer>
          )
        }}
      </Query>
    )
  }
}

export default FeedList
