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

import { LIST_USERS } from "queries/users"

import {
  page,
  pageSize,
  onChangePage,
  onChangeRowsPerPage
} from "utils/pagination"

export default class Users extends Component {
  state = {
    rowOptions: [10, 25, 50, 100],
    editableFields: ["admin"]
  }

  static defaultProps = {
    page: 1,
    pageSize: 10
  }

  handleEditClick = user => e => {
    this.setState({ user, isEditing: true })
  }

  onClose = () => this.setState({ isEditing: false })

  render() {
    return (
      <div className="admin-users px-5">
        <h2>Users</h2>
        <Query
          query={LIST_USERS}
          variables={{
            pagination: {
              page: page(this.props),
              pageSize: pageSize(this.props)
            }
          }}
        >
          {({ data, error, loading }) => {
            if (loading) return <Loader />
            if (error) return <div>Error retrieving users</div>

            const { users } = data
            const {
              meta: {
                currentPage,
                previousPage,
                nextPage,
                totalEntries,
                totalPages
              }
            } = users

            return (
              <Paper elevation={1}>
                {this.state.user && (
                  <EditRecord
                    record={this.state.user}
                    open={this.state.isEditing}
                    onClose={this.onClose}
                    editableFields={this.state.editableFields}
                  />
                )}
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>ID</TableCell>
                      <TableCell>Email</TableCell>
                      <TableCell align="right">Admin</TableCell>
                      {/* <TableCell align="right">Actions</TableCell> */}
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {users.entries.map(user => (
                      <TableRow key={user.email} hover={true}>
                        <TableCell>{user.id}</TableCell>
                        <TableCell>{user.email}</TableCell>
                        <TableCell align="right">
                          {(user.admin && "Yes") || "-"}
                        </TableCell>
                        {/* <TableCell align="right">
                            <Button
                            variant="text"
                            onClick={this.handleEditClick(user)}
                            >
                            Edit
                            </Button>
                            </TableCell> */}
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
                        rowsPerPageOptions={this.state.rowOptions}
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
