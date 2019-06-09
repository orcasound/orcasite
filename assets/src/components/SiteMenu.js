import React, { Component } from "react"
import { Link as RouterLink } from "react-router-dom"
import { Paper, Tabs, Tab, Button, Typography, Link, Grid } from "@material-ui/core"
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
    font-size: 10px;
    opacity: 0.87;
    textAlign: left;
    height: 40px;
    border-radius: 0px;
`
const GridContainer = styled.div`
  display: grid;
  grid-template-columns: 15px auto 15px;
  article {
    padding-top: 18px;
    h6 {
      color: rgba(0,0,0,0.87);
      letter-spacing: 0;
      text-align: left;
      line-height: 28px;
    }
    p {
      font-size: 14px;
      color: rgba(0,0,0,0.87);
      text-align: left;
      line-height: 20px;
    }
  }
`

const HeaderLink = styled(Link)`
  font-size: 2.2rem;
  font-weight: 400;
  letter-spacing: 1.07px;
  line-height: 35px;
  padding-top: 1.5rem;
  padding-left: 1.5rem;
  display: block;

  :hover {
    text-decoration: none;
    color: #009BDE;
  }
`
// -------------------------------------------
// Component
// -------------------------------------------
class SiteMenu extends React.Component {
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

  // maybeAbout = () => {
  //   if (this.aboutTabSelected()) {

  //     return (
  //       <Paper elevation={0} square>
  //         <Typography variant="h6"
  //         //className={classes.bodyHeader}
  //         >
  //           Listen for Whales!
  //         </Typography>
  //         <Typography
  //           component="p"
  //           paragraph={true}
  //         >
  //           Learn what orcas sound like. Then listen live for them on underwater
  //           microphones (hydrophones).
  //         </Typography>
  //         <Typography
  //           component="p"
  //           paragraph={true}
  //         >
  //           Let us know when you hear them, or any sound you think is
  //           interesting! That will help researchers and stewards protect the
  //           orcas and their environment.
  //         </Typography>
  //         <Typography
  //           component="p"
  //           paragraph={true}
  //         >
  //           You can also get notified when our listeners or algorithms detect
  //           whales at any of our hydrophone locations.
  //         </Typography>
  //       </Paper>
  //     )
  //   }
  // }

  render() {
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
          <NotificationIcon
          />
        </NotificationButton>
        <Tabs
          value={this.state.value}
          variant="fullWidth"
          indicatorColor="primary"
          onChange={this.handleChange}
          centered
        >
          <Tab
            label="About"
            component={RouterLink}
            to="/v2/about"
            fullWidth={true}
          />
          <FeedListV2 />} />
        </Tabs>
        <GridContainer>
          <div />
          {/**
          <article>
            {this.maybeAbout()}
          </article>
          */}
          <div />
        </GridContainer>
      </Paper>
    )
  }
}

export default SiteMenu
