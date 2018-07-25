import React, {Component} from 'react'
import {Link} from 'react-router-dom'

import 'styles/feed_list.scss'

export default class FeedList extends Component {
  locations = ['Seattle', 'Neah Bay', 'Port Townsend', 'Lime Kiln']

  slugify = text =>
    text
      .toString()
      .toLowerCase()
      .replace(/\s+/g, '-') // Replace spaces with -
      .replace(/[^\w\-]+/g, '') // Remove all non-word chars
      .replace(/\-\-+/g, '-') // Replace multiple - with single -
      .replace(/^-+/, '') // Trim - from start of text
      .replace(/-+$/, '') // Trim - from end of text

  render() {
    return (
      <ul className="feed-list">
        {this.locations.map((location, i) => (
          <li key={i} className="feed-item">
            <Link to={`${this.slugify(location)}`} className="d-flex align-items-center justify-content-center">
              <span>
                {location}
              </span>
            </Link>
          </li>
        ))}
      </ul>
    )
  }
}
