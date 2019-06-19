import React, { Component } from "react"
import { Query } from "react-apollo"
import { LIST_DETECTIONS } from "queries/detections"

import Paper from "@material-ui/core/Paper"
import Table from "@material-ui/core/Table"
import TableHead from "@material-ui/core/TableHead"
import TableFooter from "@material-ui/core/TableFooter"
import TableBody from "@material-ui/core/TableBody"
import TableRow from "@material-ui/core/TableRow"
import TableCell from "@material-ui/core/TableCell"

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
              <Paper elevation={1}>
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>ID</TableCell>
                      <TableCell>Feed</TableCell>
                      <TableCell>Slug</TableCell>
                      <TableCell align="right">Feed timestamp</TableCell>
                      <TableCell align="right">Player offset</TableCell>
                      <TableCell align="right">Timestamp</TableCell>
                      <TableCell align="right">Listeners</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {detections.map(detection => (
                      <TableRow key={detection.id} hover={true}>
                        <TableCell>{detection.id}</TableCell>
                        <TableCell>{detection.feed.name}</TableCell>
                        <TableCell>{detection.feed.slug}</TableCell>
                        <TableCell align="right">
                          {detection.playlistTimestamp}
                        </TableCell>
                        <TableCell align="right">
                          {Number(detection.playerOffset).toFixed(2)}
                        </TableCell>
                        <TableCell align="right">{detection.timestamp}</TableCell>
                        <TableCell align="right">
                          {detection.listenerCount || "-"}
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </Paper>
            )
          }}
        </Query>
      </div>
    )
  }
}
