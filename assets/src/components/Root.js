import React from 'react'
import { hot } from 'react-hot-loader'
import { compose } from 'react-apollo'

import { BrowserRouter, Switch, Route } from 'react-router-dom'

import Home from './Home'
import HomeV2 from  './HomeV2'
import DynamicFeed from './DynamicFeed'

const Root = (props) => {
  return (
    <BrowserRouter>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route path="/v2" component={HomeV2} />
        <Route path="/dynamic/:feedSlug" component={DynamicFeed} />
        <Route path="/:feedSlug" component={Home} />
      </Switch>
    </BrowserRouter>
  )
}

export default compose(
  hot(module),
)(Root)
