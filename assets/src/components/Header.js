import React, {Component} from 'react'
import { Link } from 'react-router-dom'

import 'styles/header.scss'

export default class Header extends Component {
  render() {
    return (
      <header className="header d-flex justify-content-center">
        <Link to="/" className="logo">Orcasound</Link>
      </header>
    )
  }
}
