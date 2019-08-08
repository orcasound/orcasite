import React, { Component } from "react"
import { Link as RouterLink } from "react-router-dom"
import {
  Paper,
  Tabs,
  Tab,
  Button,
  Typography,
  Box,
  Link,
  Grid
} from "@material-ui/core"
import styled from "styled-components"
import NotificationIcon from "@material-ui/icons/Notifications"
import FeedList from "./FeedList"

// -------------------------------------------
// Styled Components - TODO:  Move to a new file
// -------------------------------------------
const MainHeader = styled(Paper)`
  background: #000000;
  color: #ffffff;
  height: 6rem;
`

const NotificationButton = styled(Button)`
  @media screen and (min-width: 599px) {
    font-size: 14px;
    opacity: 0.87;
    textalign: left;
    height: 40px;
    border-radius: 0px;
  }

  font-size: 10px;
  opacity: 0.87;
  textalign: left;
  height: 40px;
  border-radius: 0px;
`

const HeaderLink = styled(Link)`
  font-size: 2.2rem;
  font-weight: 400;
  letter-spacing: 1.07px;
  line-height: 35px;
  padding-top: 1.5rem;
  padding-left: 3.5rem;
  display: block;

  :hover {
    text-decoration: none;
    color: #009bde;
  }
`
// -------------------------------------------
// Component
// -------------------------------------------
class SiteMenu extends Component {
  state = {
    value: 0
  }

  handleChange = (event, value) => {
    console.log(value)
    this.setState({ value })
  }

  aboutTabSelected = () => {
    return this.state.value == 0
  }

  render() {
    return (
      <Paper elevation={0} square>
        <MainHeader square elevation={0}>
          <HeaderLink
            component={RouterLink}
            to="/"
            color="inherit"
            variant="h6"
            underline="none"
          >
            <Typography component="h1" variant="h4">
              <Box>Orcasound</Box>
            </Typography>
          </HeaderLink>
        </MainHeader>
        <NotificationButton
          variant="contained"
          centered="true"
          fullWidth={true}
          color="primary"
        >
          Get notified when there's whale activity
          <NotificationIcon />
        </NotificationButton>
        <Tabs
          value={this.state.value}
          variant="fullWidth"
          indicatorColor="primary"
          onChange={this.handleChange}
          centered
        >
          <Tab label="About" component={RouterLink} to="/" fullWidth={true} />
          <FeedList />
        </Tabs>
      </Paper>
    )
  }
}

export default SiteMenu
