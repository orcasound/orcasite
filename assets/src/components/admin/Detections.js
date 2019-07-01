import React, { Component } from "react"

import Paper from "@material-ui/core/Paper"
import Table from "@material-ui/core/Table"
import TableHead from "@material-ui/core/TableHead"
import TableFooter from "@material-ui/core/TableFooter"
import TableBody from "@material-ui/core/TableBody"
import TableRow from "@material-ui/core/TableRow"
import TableCell from "@material-ui/core/TableCell"
import TablePagination from "@material-ui/core/TablePagination"

import Loader from "components/Loader"

import { formatTimestamp } from "utils/utils"

export default class Detections extends Component {
  render() {
    const { detections, feed } = this.props
    return (
      <div className="admin-detections">
        <h2>Detections</h2>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>Node</TableCell>
              <TableCell>Listeners</TableCell>
              <TableCell align="right">Timestamp</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {detections.map(detection => (
              <TableRow key={detection.id} hover={true}>
                <TableCell>{detection.id}</TableCell>
                <TableCell>{feed.slug}</TableCell>
                <TableCell>{detection.listenerCount}</TableCell>
                <TableCell align="right" title={detection.timestamp}>
                  {formatTimestamp(detection.timestamp)}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </div>
    )
  }
}
