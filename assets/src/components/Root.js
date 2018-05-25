import React from 'react'
import { hot } from 'react-hot-loader'
import { compose } from 'react-apollo'

import { BrowserRouter, Switch, Route } from 'react-router-dom'

import Home from './Home'

const Root = (props) => {
  return (
    <BrowserRouter>
      <Switch>
        <Route exact path="/" component={Home} />
      </Switch>
    </BrowserRouter>
  )
}

export default compose(
  hot(module),
)(Root)
