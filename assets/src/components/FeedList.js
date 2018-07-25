import React, {Component} from 'react'
import {Link} from 'react-router-dom'

import {arrayOf} from 'prop-types'
import {feedType} from 'types/feedType'

import 'styles/feed_list.scss'

export default class FeedList extends Component {
  static propTypes = {
    feeds: arrayOf(feedType)
  }

  render() {
    const {feeds} = this.props
    return (
      <ul className="feed-list">
        {feeds.map((feed, i) => (
          <li key={i} className="feed-item">
            <Link to={`${feed.slug}`} className="d-flex align-items-center justify-content-center">
              <span>
                {feed.name}
              </span>
            </Link>
          </li>
        ))}
      </ul>
    )
  }
}
