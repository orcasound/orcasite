import React from 'react'
import { hot } from 'react-hot-loader'
import { compose } from 'react-apollo'

import { BrowserRouter, Switch, Route } from 'react-router-dom'

import Home from './Home'
import Admin from './admin/Dashboard'
import DynamicFeed from './DynamicFeed'

const Root = (props) => {
  return (
    <BrowserRouter>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route path="/dynamic/:feedSlug" component={DynamicFeed} />

        {/* TODO: Change to AdminRoute once auth is in */}
        <Route path="/admin" component={Admin} />

        <Route path="/:feedSlug" component={Home} />
      </Switch>
    </BrowserRouter>
  )
}

export default compose(
  hot(module),
)(Root)
