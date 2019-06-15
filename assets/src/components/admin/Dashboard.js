import React, { Component } from "react"
import { Route, Link as RouterLink } from "react-router-dom"

import Link from "@material-ui/core/Link"
import Paper  from "@material-ui/core/Paper"
import JssProvider from "react-jss/lib/JssProvider"
import { create } from "jss"
import {
  createGenerateClassName,
  jssPreset,
  createMuiTheme,
  MuiThemeProvider
} from "@material-ui/core/styles"
import styled from "styled-components"

import Detections from './Detections'

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
  background: #000000;
  color: #ffffff;
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

  :hover {
    text-decoration: none;
    color: #009bde;
  }
`
// ---------------------------------------
// Ordering style sheets in the <head>
// ---------------------------------------
const generateClassName = createGenerateClassName()
const jss = create({
  ...jssPreset(),
  // Define a custom insertion for injecting the JSS styles in the DOM
  insertionPoint: "jss-insertion-point"
})

export default class Dashboard extends Component {
  render() {
    return (
      <JssProvider jss={jss} generateClassName={generateClassName}>
        <MuiThemeProvider theme={theme}>
          <Paper square elevation={0}>
            <MainHeader square elevation={0}>
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
            <h1 className="px-5 my-3">Admin</h1>
            <Route exact path='/admin' component={Detections} />
          </Paper>
        </MuiThemeProvider>
      </JssProvider>
    )
  }
}
