import { hot } from 'react-hot-loader/root'

import React from 'react'
import { BrowserRouter, Switch, Route } from 'react-router-dom'

import Home from './Home'
import Admin from './admin/Dashboard'
import HomeV2 from './HomeV2'
import DynamicFeed from './DynamicFeed'

const Root = (props) => {
  return (
    <BrowserRouter>
      <Switch>
        <Route exact path="/" component={Home} />
        {/* <Route exact path="/v2" component={HomeV2} /> */}
        {/* <Route path="v2/about" component={HomeV2} /> */}
        {/* <Route path="/v2/:feedSlug" component={HomeV2} /> */}
        <Route path="/dynamic/:feedSlug" component={DynamicFeed} />

        {/* TODO: Change to AdminRoute once auth is in */}
        <Route path="/admin" component={Admin} />

        <Route path="/:feedSlug" component={Home} />
      </Switch>
    </BrowserRouter>
  )
}

export default hot(Root)
