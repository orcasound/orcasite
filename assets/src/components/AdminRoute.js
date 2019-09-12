import React from "react"
import { Route, Redirect } from "react-router-dom"

import { Query } from "react-apollo"
import gql from "graphql-tag"
import { getToken } from "utils/auth"

import Loader from "components/Loader"

const AdminRoute = ({ component: Component, ...props }) => (
  <Query query={GET_ADMIN} skip={!getToken()} fetchPolicy={"network-only"}>
    {({ loading, error, data }) => {
      if (loading)
        return (
          <div className="page-loader">
            <Loader />
          </div>
        )

      const { currentUser } = data || {}
      if (error || typeof data === "undefined" || !currentUser.admin)
        return (
          <Redirect
            to={{ pathname: "login", state: { from: props.location } }}
          />
        )

      return (
        <Route
          {...props}
          render={props => {
            return <Component {...props} data={data} />
          }}
        />
      )
    }}
  </Query>
)

export default AdminRoute

const GET_ADMIN = gql`
  query adminGetUser {
    currentUser {
      id
      admin
      email
      auth_token
    }
  }
`
