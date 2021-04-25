import { hot } from "react-hot-loader/root"

import React from "react"
import { Router, Switch, Route } from "react-router-dom"
import { createBrowserHistory } from 'history'
import ReactGA from 'react-ga'

import AdminRoute from "./AdminRoute"

import Home from "./Home"
import Admin from "./admin/Dashboard"
import DynamicFeed from "./DynamicFeed"
import Login from "./Login"

const history = createBrowserHistory()
history.listen((location, action) => {
  ReactGA.set({ page: location.pathname });
  ReactGA.pageview(location.pathname + location.search);
});

const Root = props => {
  return (
    <Router history={history}>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route
          exact
          path="/login"
          render={props => <Login {...props} login={true} />}
        />
        <Route
          exact
          path="/register"
          render={props => <Login {...props} login={false} />}
        />
        <Route path="/dynamic/:feedSlug" component={DynamicFeed} />

        <AdminRoute path="/admin" component={Admin} />

        <Route path="/:feedSlug" component={Home} />
      </Switch>
    </Router>
  )
}

export default hot(Root)
