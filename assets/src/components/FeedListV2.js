import React, { Component } from "react"
import { Button, ClickAwayListener, Grow, Paper, Popper, MenuList, MenuItem } from "@material-ui/core"
import { ArrowDropDown, Person } from "@material-ui/icons"
import { withStyles } from "@material-ui/core/styles"

const styles = {
  root: {
    display: "flex",
    flexDirection: "column",
    justifyContent: "space-between",
    flexGrow: 1,
    maxWidth: "none",
    flexShrink: 1
  },
  button: {
    display: "flex",
    flexGrow: 1,
    maxWidth: "none",
    flexShrink: 1
  },
  menuList: {
    fontSize: 14,
    textAlign: "center",
  },
  menuItem: {
    display: "flex",
    justifyContent: "space-between",
  }
};

class FeedListV2 extends Component {
  state = {
    open: false,
    bushPointUsers: 1,
    haroStraitUsers: 3
  }

  handleToggle = () => {
    this.setState(state => ({ open: !state.open }))
  }

  handleClose = event => {
    if (this.anchorEl.contains(event.target)) {
      return
    }

    this.setState({ open: false })
  }

  render() {
    const { classes } = this.props
    const { open, bushPointUsers, haroStraitUsers } = this.state

    return (
      <div className={classes.root} >
        <Button
          className={classes.button}
          buttonRef={node => {
            this.anchorEl = node;
          }}
          aria-owns={open ? "menu-list-grow" : undefined}
          aria-haspopup="true"
          onClick={this.handleToggle}
        >
          Listen Live<ArrowDropDown />
        </Button>
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
                    <MenuItem
                      className={classes.menuItem}
                      onClick={this.handleClose}>
                      <span>Bush Point</span>
                      <div>{bushPointUsers}<Person /></div>
                    </MenuItem>
                    <MenuItem
                      className={classes.menuItem}
                      onClick={this.handleClose}>
                      <span>Haro Strait/<br>
                      </br>OrcaSound Lab</span>
                      <div>{haroStraitUsers}<Person /></div>
                    </MenuItem>
                  </MenuList>
                </ClickAwayListener>
              </Paper>
            </Grow>
          )}
        </Popper>
      </div>
    )
  }
}

export default withStyles(styles)(FeedListV2)
