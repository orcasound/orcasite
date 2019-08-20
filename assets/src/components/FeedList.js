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
  Box
} from "@material-ui/core"
import { ArrowDropDown, Person } from "@material-ui/icons"
import styled from "styled-components"

import { LIST_FEEDS } from "../queries/feeds"

const FeedListGridContainer = styled(Paper)`
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
  flex-shrink: 1;
  max-width: none;
  padding: 0.375rem 0rem 0.375rem 0rem;
  height: 3rem;
`

const FeedMenuItem = styled(MenuItem)`
  display: flex;
  justify-content: space-between;
  color: #000000;
  a {
    color: rgba(0, 0, 0, 0.87);
  }
`

const FeedList = props => {
  const [open, setOpen] = useState(false)
  const anchorRef = React.useRef(null)

  const handleToggle = () => {
    setOpen(prevOpen => !prevOpen)
  }

  const handleClose = event => {
    if (anchorRef.current && anchorRef.current.contains(event.target)) {
      return
    }

    setOpen(false)
  }

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
          <FeedListGridContainer elevation={0}>
            <ClickAwayListener onClickAway={handleClose}>
              <div>
                <FeedButton
                  ref={anchorRef}
                  aria-controls="menu-list-grow"
                  aria-haspopup="true"
                  variant="text"
                  onClick={handleToggle}
                >
                  <Box letterSpacing="0.03572em">Listen Live</Box>
                  <ArrowDropDown />
                </FeedButton>
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
                          placement === "bottom"
                            ? "center top"
                            : "center bottom"
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
              </div>
            </ClickAwayListener>
          </FeedListGridContainer>
        )
      }}
    </Query>
  )
}

FeedList.defaultProps = {
  bushPointUsers: 1,
  haroStraitUsers: 3
}

export default FeedList

// const FeedList = React.forwardRef((props, ref) => {
//   let [anchorEl, setAnchorEl] = useState(null)
//   let open = Boolean(anchorEl)
//   const id = open ? "popper" : undefined

//   const handleClick = event => {
//     setAnchorEl(anchorEl ? null : event.currentTarget)
//   }

//   const handleClose = event => {
//     if (anchorEl.contains(event.target)) {
//       return
//     }
//     setAnchorEl(prev => !prev)
//   }

//   const { bushPointUsers, haroStraitUsers } = props

//   return (
//     <Query query={LIST_FEEDS}>
//       {({ data, loading, error }) => {
//         if (loading || error) {
//           return (
//             <ul>
//               <li>
//                 <div>LOADING</div>
//               </li>
//             </ul>
//           )
//         }

//         const { feeds } = data
//         console.log("open", open)
//         return (
//           <FeedListGridContainer>
//             <FeedButton
//               aria-haspopup="true"
//               aria-describedby={id}
//               variant="text"
//               onClick={handleClick}
//             >
//               <Box letterSpacing="0.03572em">Listen Live</Box>
//               <ArrowDropDown />
//             </FeedButton>
//             <Popper
//               id={id}
//               open={open}
//               anchorEl={anchorEl}
//               keepMounted
//               transition
//             >
//               {({ TransitionProps, placement }) => (
//                 <ClickAwayListener onClickAway={handleClose}>
//                   <Grow
//                     {...TransitionProps}
//                     id="menu-list-grow"
//                     style={{
//                       transformOrigin:
//                         placement === "bottom" ? "center top" : "center bottom"
//                     }}
//                   >
//                     <Paper>
//                       <MenuList>
//                         {feeds
//                           .slice()
//                           .reverse()
//                           .map((feed, i) => {
//                             return (
//                               <FeedMenuItem key={i}>
//                                 <Link to={`/${feed.slug}`}>
//                                   <span>{feed.name}</span>
//                                   <div>
//                                     {bushPointUsers}
//                                     <Person />
//                                   </div>
//                                 </Link>
//                               </FeedMenuItem>
//                             )
//                           })}
//                       </MenuList>
//                     </Paper>
//                   </Grow>
//                 </ClickAwayListener>
//               )}
//             </Popper>
//           </FeedListGridContainer>
//         )
//       }}
//     </Query>
//   )
// })

// FeedList.defaultProps = {
//   bushPointUsers: 1,
//   haroStraitUsers: 3
// }

// export default FeedList
