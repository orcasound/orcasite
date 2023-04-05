import React, { Component } from "react"

import CircularProgress from "@material-ui/core/CircularProgress"

export default class Loader extends Component {
  render() {
    return (
      <div className="w-100 h-100 d-flex justify-content-center align-items-center">
        <CircularProgress size={80} />
      </div>
    )
  }
}
