import React, {Component} from 'react'
import {Link} from 'react-router-dom'

import { library } from '@fortawesome/fontawesome-svg-core'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import {faGithub} from '@fortawesome/free-brands-svg-icons'

import 'styles/header.scss'
export default class Header extends Component {
  render() {
      console.log(faGithub)
    return (
      <header className="header d-flex justify-content-between align-items-center">
        <Link to="/" className="logo">
          Orcasound
        </Link>
        <Link
          to="https://github.com/orcasound"
          target="_blank"
          className="btn btn-outline-primary">
          <FontAwesomeIcon icon={faGithub} className="mr-2" />
          Github
        </Link>
      </header>
    )
  }
}
