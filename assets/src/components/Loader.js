import React, {Component} from 'react'

import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faSpinner} from '@fortawesome/free-solid-svg-icons'

export default class Loader extends Component {
  render() {
    return (
      <div className="w-100 h-100 d-flex justify-content-center align-items-center">
        <FontAwesomeIcon icon={faSpinner} spin />
      </div>
    )
  }
}
