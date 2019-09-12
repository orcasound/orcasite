import React, { Component } from "react"
import Drawer from "@material-ui/core/Drawer"

const displayFields = ({ record }) =>
  Object.keys(record).filter(name => name !== "__typename")

export default function EditRecord(props) {
  return (
    <Drawer open={props.open} onClose={props.onClose} classes={{ root: "p-3" }}>
      <h1>{props.record["__typename"]}</h1>
      {displayFields(props).join(", ")}
    </Drawer>
  )
}
