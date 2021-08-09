import React, { Component } from "react"
import { Query } from "react-apollo"

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
import EditRecord from "./EditRecord"

import { LIST_FEEDS } from "queries/feeds"

import {
  page,
  pageSize,
  onChangePage,
  onChangeRowsPerPage
} from "utils/pagination"

export default class Feeds extends Component {
  state = {
    rowOptions: [10, 25, 50, 100],
    editableFields: ["admin"]
  }

  static defaultProps = {
    page: 1,
    pageSize: 10
  }

  handleEditClick = feed => e => {
    this.setState({ feed, isEditing: true })
  }

  onClose = () => this.setState({ isEditing: false })

  render() {
    return (
      <div className="admin-feeds px-5">
        <h2>Feeds</h2>
        <Query query={LIST_FEEDS}>
          {({ data, error, loading }) => {
            if (loading) return <Loader />
            if (error) return <div>Error retrieving feeds</div>

            const { feeds } = data
            return (
              <Paper elevation={1}>
                {this.state.feed && (
                  <EditRecord
                    record={this.state.feed}
                    open={this.state.isEditing}
                    onClose={this.onClose}
                    editableFields={this.state.editableFields}
                  />
                )}
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>ID</TableCell>
                      <TableCell>Name</TableCell>
                      <TableCell>Slug</TableCell>
                      <TableCell>Node name</TableCell>
                      {/* <TableCell align="right">Actions</TableCell> */}
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {feeds
                      .slice()
                      .sort((a, b) => Number(b.id) - Number(a.id))
                      .map(feed => (
                        <TableRow key={feed.id} hover={true}>
                          <TableCell>{feed.id}</TableCell>
                          <TableCell>{feed.name}</TableCell>
                          <TableCell>{feed.slug}</TableCell>
                          <TableCell>{feed.nodeName}</TableCell>
                          {/* <TableCell align="right">
                            <Button
                            variant="text"
                            onClick={this.handleEditClick(feed)}
                            >
                            Edit
                            </Button>
                            </TableCell> */}
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
