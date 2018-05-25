import React, { Component } from 'react'

import Player from './Player'

import { gql } from 'apollo-boost'
import { Query } from 'react-apollo'

import 'styles/home.scss'

export default class Home extends Component {
  render() {
    return (
      <div className="home">
        <Player />
      </div>
    )
  }
}
