import React, { Component } from "react"
import { Query } from "react-apollo"
import { LIST_DETECTIONS } from "queries/detections"

import Loader from "components/Loader"

export default class Detections extends Component {
  render() {
    return (
      <div className="admin-detections px-5">
        <h2>Detections</h2>
        <Query query={LIST_DETECTIONS}>
          {({ data, error, loading }) => {
            if (loading) return <Loader />
            if (error) return <div>Error retrieving detections</div>

            const { detections } = data
            return (
              <table className="table">
                <thead>
                  <tr>
                    <th>ID</th>
                    <th>Feed</th>
                    <th>Slug</th>
                    <th className="text-right">Feed timestamp</th>
                    <th className="text-right">Player offset</th>
                    <th className="text-right">Timestamp</th>
                    <th className="text-right">Listeners</th>
                  </tr>
                </thead>
                <tbody>
                  {detections.map(detection => (
                    <tr key={detection.id}>
                      <td>{detection.id}</td>
                      <td>{detection.feed.name}</td>
                      <td>{detection.feed.slug}</td>
                      <td className="text-right">
                        {detection.playlistTimestamp}
                      </td>
                      <td className="text-right">
                        {Number(detection.playerOffset).toFixed(2)}
                      </td>
                      <td className="text-right">{detection.timestamp}</td>
                      <td className="text-right">
                        {detection.listenerCount || "-"}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            )
          }}
        </Query>
      </div>
    )
  }
}
