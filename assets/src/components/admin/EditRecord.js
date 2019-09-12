import React, { Component } from "react"
import Drawer from "@material-ui/core/Drawer"
import Box from "@material-ui/core/Box"

const displayFields = ({ record }) =>
  Object.keys(record).filter(name => name !== "__typename")

export default function EditRecord(props) {
  return (
    <Drawer open={props.open} onClose={props.onClose}>
      <Box p={3}>
        <h1>{props.record["__typename"]}</h1>
        {displayFields(props).map(field => (
          <div key={field}>
            <div>{field}</div>
            <div>{props.record[field]}</div>
            <div>{typeof props.record[field]}</div>
          </div>
        ))}
      </Box>
    </Drawer>
  )
}
