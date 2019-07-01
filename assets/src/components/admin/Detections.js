import React, { Component } from "react"
import { Query } from "react-apollo"
import { LIST_CANDIDATES } from "queries/detections"

import Paper from "@material-ui/core/Paper"
import Table from "@material-ui/core/Table"
import TableHead from "@material-ui/core/TableHead"
import TableFooter from "@material-ui/core/TableFooter"
import TableBody from "@material-ui/core/TableBody"
import TableRow from "@material-ui/core/TableRow"
import TableCell from "@material-ui/core/TableCell"
import TablePagination from "@material-ui/core/TablePagination"
import Button from "@material-ui/core/Button"

import Loader from "components/Loader"

export default class Detections extends Component {
  state = {
    page: 1,
    pageSize: 10,
    rowOptions: [10, 25, 50, 100]
  }

  handleGroupClick = detectionGroup => () =>
    console.log("Clicked", detectionGroup)

  // MUI uses 0-indexing for pages, must add offset
  onChangePage = offset => (event, page) =>
    this.setState({ page: page + offset })
  onChangeRowsPerPage = ({ target: { value } }) =>
    this.setState({ pageSize: value })

  render() {
    const { page, pageSize, rowOptions } = this.state
    return (
      <div className="admin-detections px-5">
        <h2>Detections</h2>
        <Query
          query={LIST_CANDIDATES}
          variables={{ pagination: { page: page, pageSize: pageSize } }}
        >
          {({ data, error, loading }) => {
            if (loading) return <Loader />
            if (error) return <div>Error retrieving detections</div>

            const { candidates } = data
            const {
              meta: {
                currentPage,
                previousPage,
                nextPage,
                totalEntries,
                totalPages
              }
            } = candidates
            return (
              <Paper elevation={1}>
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>ID</TableCell>
                      <TableCell>Node</TableCell>
                      <TableCell align="right">Detections</TableCell>
                      <TableCell align="right">Timestamp</TableCell>
                      <TableCell align="center">Actions</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {candidates.entries.map(candidate => (
                      <TableRow key={candidate.id} hover={true}>
                        <TableCell>{candidate.id}</TableCell>
                        <TableCell>{candidate.feed.slug}</TableCell>
                        <TableCell align="right">
                          {candidate.detectionCount}
                        </TableCell>
                        <TableCell align="right">{candidate.minTime}</TableCell>
                        <TableCell align="center">
                          <Button onClick={this.handleGroupClick(candidate)}>
                            View
                          </Button>
                        </TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                  <TableFooter>
                    <TableRow>
                      <TablePagination
                        count={totalEntries}
                        page={currentPage - 1}
                        rowsPerPage={pageSize}
                        onChangePage={this.onChangePage(1)}
                        onChangeRowsPerPage={this.onChangeRowsPerPage}
                        rowsPerPageOptions={rowOptions}
                      />
                    </TableRow>
                  </TableFooter>
                </Table>
              </Paper>
            )
          }}
        </Query>
      </div>
    )
  }
}
