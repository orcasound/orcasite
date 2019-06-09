import React, {Component} from 'react'
import Header from 'components/Header'

export default class Dashboard extends Component {
  render() {
    return <div className="admin">
      <Header />
      <h1>Admin</h1>
      {JSON.stringify(this.props)}
    </div>
  }

}
