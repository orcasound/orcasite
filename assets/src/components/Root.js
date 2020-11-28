import { hot } from "react-hot-loader/root"

import React from "react"
import { BrowserRouter, Switch, Route } from "react-router-dom"

import AdminRoute from "./AdminRoute"

import Home from "./Home"
import Admin from "./admin/Dashboard"
import DynamicFeed from "./DynamicFeed"
import Login from "./Login"
import GoogleAnalytics from "./GoogleAnalytics"

const Root = props => {
  return (
    <BrowserRouter>
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

        <Route exact path="/newhome" component={NewHome} />  
        <Route exact path="/experimentgql" component={GQLExperiment} />  
        
        <Route path="/dynamic/:feedSlug" component={DynamicFeed} />

        <AdminRoute path="/admin" component={Admin} />

        <Route path="/listen" component={ListenPageRoot}/>
        <Route path="/learn" component={LearnPageRoot}/>
        <Route path="/projects" component={ProjectsPageRoot}/>
        <Route path="/blogs" component={BlogsPageRoot}/>
        <Route path="/about" component={AboutPageRoot}/>
        <Route path="/support" component={SupportPageRoot}/>
        <Route path="/subscribe" component={SubscribePageRoot}/>


        <Route path="/:feedSlug" component={Home} />

      </Switch>
      <GoogleAnalytics />
    </BrowserRouter>  
    )
}

export default hot(Root)
