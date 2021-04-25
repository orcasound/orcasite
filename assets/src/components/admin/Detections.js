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
import DetectionPlayer from "components/DetectionPlayer"

import { formatTimestamp } from "utils/utils"

const offsetPadding = 5

export default class Detections extends Component {
  minOffset = detections => Math.min(...detections.map(d => +d.playerOffset))
  maxOffset = detections => Math.max(...detections.map(d => +d.playerOffset))

  render() {
    const { detections, feed } = this.props
    const startOffset = Math.max(0, this.minOffset(detections) - offsetPadding)
    const endOffset = this.maxOffset(detections) + offsetPadding
    return (
      <div className="admin-detections">
        <h2>Detections</h2>
        <DetectionPlayer
          feed={feed}
          marks={detections.map(d => ({
            label: d.id,
            value: (+d.playerOffset - +startOffset).toFixed(1)
          }))}
          timestamp={detections[0].playlistTimestamp}
          startOffset={startOffset}
          endOffset={endOffset}
        />

        <Table>
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>Node</TableCell>
              <TableCell>Listeners</TableCell>
              <TableCell>Description</TableCell>
              <TableCell align="right">Timestamp</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {detections
              .slice()
              .sort((a, b) => b.id - a.id)
              .map(detection => (
                <TableRow key={detection.id} hover={true}>
                  <TableCell>{detection.id}</TableCell>
                  <TableCell>{feed.slug}</TableCell>
                  <TableCell>{detection.listenerCount}</TableCell>
                  <TableCell>{detection.description}</TableCell>
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
