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
import Modal from "@material-ui/core/Modal"

import Loader from "components/Loader"
import Detections from "components/admin/Detections"

import { formatTimestamp } from "utils/utils"
import {
  page,
  pageSize,
  onChangePage,
  onChangeRowsPerPage
} from "utils/pagination"

export default class Candidates extends Component {
  state = {
    rowOptions: [10, 25, 50, 100],
    detectionModalOpen: false,
    detections: [],
    feed: null
  }

  static defaultProps = {
    page: 1,
    pageSize: 10
  }

  handleCandidateClick = ({ detections, feed }) => () =>
    this.setState({
      detectionModalOpen: true,
      detections,
      feed
    })

  handleModalClose = () =>
    this.setState({ detectionModalOpen: false, detections: [] })

  render() {
    const { rowOptions, detectionModalOpen, detections, feed } = this.state
    return (
      <div className="admin-candidates px-5">
        <h2>Candidates</h2>
        <Query
          query={LIST_CANDIDATES}
          variables={{
            pagination: {
              page: page(this.props),
              pageSize: pageSize(this.props)
            }
          }}
        >
          {({ data, error, loading }) => {
            if (loading) return <Loader />
            if (error) return <div>Error retrieving candidates</div>

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
                <Modal
                  open={detectionModalOpen}
                  onClose={this.handleModalClose}
                  className="p-4"
                >
                  <Paper classes={{ root: "p-5" }}>
                    <Detections detections={detections} feed={feed} />
                  </Paper>
                </Modal>
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>ID</TableCell>
                      <TableCell>Node</TableCell>
                      <TableCell align="right">Detections</TableCell>
                      <TableCell align="right">Timestamp</TableCell>
                      <TableCell align="right">Descriptions</TableCell>
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
                        <TableCell align="right" title={candidate.minTime}>
                          {formatTimestamp(candidate.minTime)}
                        </TableCell>
                        <TableCell align="right" title={candidate.minTime}>
                          {candidate.detections
                            .map(d => d.description)
                            .filter(d => typeof d !== "undefined" && d !== null)
                            .slice(0, 3)
                            .join(", ")}
                        </TableCell>
                        <TableCell align="center">
                          <Button
                            onClick={this.handleCandidateClick(candidate)}
                          >
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
                        rowsPerPage={pageSize(this.props)}
                        onChangePage={onChangePage(this.props, 1)}
                        onChangeRowsPerPage={onChangeRowsPerPage(this.props)}
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
