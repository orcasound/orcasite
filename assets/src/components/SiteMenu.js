import React, { Component } from "react"
import { Link as RouterLink } from "react-router-dom"
import {
  Paper,
  Tabs,
  Tab,
  Button,
  Typography,
  Link,
  Grid
} from "@material-ui/core"
import styled from "styled-components"
import NotificationIcon from "@material-ui/icons/Notifications"
import FeedListV2 from "./FeedListV2"

// -------------------------------------------
// Styled Components - TODO:  Move to a new file
// -------------------------------------------
const MainHeader = styled(Paper)`
  background: #000000;
  color: #ffffff;
  height: 80px;
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
    console.log("inner width", innerWidth)
    return (
      <Paper elevation={0} square>
        <MainHeader square elevation={0}>
          <HeaderLink
            component={RouterLink}
            to="/v2"
            color="inherit"
            variant="h1"
            underline="none"
          >
            Orcasound
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
          <Tab label="About" component={RouterLink} to="/v2" fullWidth={true} />
          <FeedListV2 />
        </Tabs>
      </Paper>
    )
  }
}

export default SiteMenu
