import React, { Component } from "react"
import { Button, ClickAwayListener, Grow, Paper, Popper, MenuList, MenuItem } from "@material-ui/core"
import { ArrowDropDown, Person } from "@material-ui/icons"
import styled from "styled-components"

// -------------------------------------------
// Styled Components - TODO:  Move to a new file
// -------------------------------------------
const FeedListContainer = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  flex-grow: 1;
  max-width: none;
  flex-shrink: 1;
`

const FeedButton = styled(Button)`
  display: flex;
  flex-grow: 1;
  max-width: none;
  flex-shrink: 1;
  box-shadow: 0 25px 25px rgba(192, 192, 192, 0.5);
`

const FeedMenuItem = styled(MenuItem)`
  display: flex;
  justify-content: space-between;
`
// -------------------------------------------
// Component
// -------------------------------------------
class FeedListV2 extends Component {
  constructor(props) {
    super(props);

    this.handleToggle = this.handleToggle.bind(this);
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
      <FeedListContainer>
        <FeedButton
          buttonRef={node => {
            this.anchorEl = node;
          }}
          aria-owns={open ? "menu-list-grow" : undefined}
          aria-haspopup="true"
          onClick={this.handleToggle}
        >
          Listen Live<ArrowDropDown />
        </FeedButton>
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
              style={{ transformOrigin: placement === "bottom" ? "center top" : "center bottom" }}
            >
              <Paper>
                <ClickAwayListener onClickAway={this.handleClose}>
                  <MenuList>
                    <FeedMenuItem
                      //className={classes.menuItem}
                      onClick={this.handleClose}>
                      <span>Bush Point</span>
                      <div>{bushPointUsers}<Person /></div>
                    </FeedMenuItem>
                    <FeedMenuItem
                      //className={classes.menuItem}
                      onClick={this.handleClose}>
                      <span>Haro Strait/<br>
                      </br>OrcaSound Lab</span>
                      <div>{haroStraitUsers}<Person /></div>
                    </FeedMenuItem>
                  </MenuList>
                </ClickAwayListener>
              </Paper>
            </Grow>
          )}
        </Popper>
      </FeedListContainer>
    )
  }
}

export default FeedListV2
