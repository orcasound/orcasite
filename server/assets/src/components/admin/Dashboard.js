import React, { Component } from "react"
import { Route, Link as RouterLink } from "react-router-dom"

import Link from "@material-ui/core/Link"
import Paper from "@material-ui/core/Paper"
import Container from "@material-ui/core/Container"
import { createMuiTheme, MuiThemeProvider } from "@material-ui/core/styles"
import { StylesProvider } from "@material-ui/styles"
import styled from "styled-components"

import Candidates from "./Candidates"
import Users from "./Users"
import Feeds from "./Feeds"

const theme = createMuiTheme({
  /* change default theme options below */
  /* gets merged into custom style objects */
  /* using latest version of typography */
  typography: {
    useNextVariants: true
  },
  palette: {
    primary: {
      main: "#2196f3"
    },
    secondary: {
      main: "#009688"
    }
  }
})

const MainHeader = styled(Paper)`
  background: #011c28;
  color: #f4f4f0;
  height: 80px;
`

const HeaderLink = styled(Link)`
  font-size: 2.2rem;
  font-weight: 400;
  letter-spacing: 1.07px;
  line-height: 35px;
  padding-top: 1.5rem;
  padding-left: 1.5rem;
  display: block;

  &:hover {
    text-decoration: none;
    color: #009bde;
  }
`

const adminLinks = [
  { name: "Admin", path: "/admin" },
  { name: "Candidates", path: "/admin/candidates" },
  { name: "Users", path: "/admin/users" },
  { name: "Feeds", path: "/admin/feeds" }
]

const Dashboard = props => {
  return (
    <StylesProvider injectFirst>
      <MuiThemeProvider theme={theme}>
        <Paper square elevation={0}>
          <MainHeader square elevation={1}>
            <HeaderLink
              component={RouterLink}
              to="/"
              color="inherit"
              variant="h1"
              underline="none"
            >
              Orcasound
            </HeaderLink>
          </MainHeader>
          <Paper square elevation={0}>
            <Container>
              {adminLinks.map(({ name, path }) => (
                <Link
                  key={path}
                  to={path}
                  component={RouterLink}
                  classes={{ root: "mx-2" }}
                >
                  {name}
                </Link>
              ))}
            </Container>
          </Paper>
          <Route exact path="/admin" component={Candidates} />
          <Route
            path={["/admin/candidates/:id", "/admin/candidates"]}
            component={Candidates}
          />
          <Route path={["/admin/users"]} component={Users} />
          <Route path={["/admin/feeds"]} component={Feeds} />
        </Paper>
      </MuiThemeProvider>
    </StylesProvider>
  )
}

export default Dashboard
