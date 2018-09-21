import React, {Component} from 'react'
import {Link} from 'react-router-dom'
import {Query} from 'react-apollo'

import 'styles/feed_list.scss'

import {LIST_FEEDS} from 'queries/feeds'

export default class FeedList extends Component {
  render() {
    return (
      <Query query={LIST_FEEDS}>
        {({data, loading, error}) => {
          if (loading) {
            return (
              <ul className="feed-list">
                <li className="feed-item loading">
                  <div className="feed-item-loader"></div>
                </li>
              </ul>
            )
          }

          const {feeds} = data
          return (
            <ul className="feed-list">
              {feeds.map((feed, i) => (
                <li key={i} className="feed-item">
                  <Link
                    to={`/${feed.slug}`}
                    className="d-flex align-items-center justify-content-center text-center">
                    <span>{feed.name}</span>
                  </Link>
                </li>
              ))}
            </ul>
          )
        }}
      </Query>
    )
  }
}
