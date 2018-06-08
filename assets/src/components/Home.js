import React, { Component } from 'react'

import Player from './Player'

import { gql } from 'apollo-boost'
import { Query } from 'react-apollo'

import 'styles/home.scss'

export default class Home extends Component {
  render() {
    var {nodeName} = this.props.match.params
    nodeName = nodeName || "rpi_seattle"

    return (
      <div className="home">
        <Player nodeName={nodeName} />
      </div>
    )
  }

  componentDidMount() {
    document.title = "Orcasound Beta";
  }
}
