import React, { Component } from "react"
import Drawer from "@material-ui/core/Drawer"
import Box from "@material-ui/core/Box"
import { Formik, Field } from "formik"

const displayFields = ({ record }) =>
  Object.keys(record).filter(name => name !== "__typename")

const FieldInput = ({ name, value }) => {
  switch (typeof value) {
    case "string":
      return <Field component="input" value={value} name={name} />
    case "boolean":
      return <Checkbox value={name} name={name} />
  }
}

const Checkbox = props => (
  <Field name={props.name}>
    {({ field, form }) => (
      <label>
        <input
          type="checkbox"
          {...props}
          checked={field.value.includes(props.value)}
          onChange={() => {
            if (field.value.includes(props.value)) {
              const nextValue = field.value.filter(
                value => value !== props.value
              )
              form.setFieldValue(props.name, nextValue)
            } else {
              const nextValue = field.value.concat(props.value)
              form.setFieldValue(props.name, nextValue)
            }
          }}
        />
        {props.value}
      </label>
    )}
  </Field>
)

export default function EditRecord(props) {
  return (
    <Drawer open={props.open} onClose={props.onClose}>
      <Box p={3} style={{ minWidth: "300px" }}>
        <Formik
          onSubmit={values => console.log(JSON.stringify(values, null, 2))}
        >
          {formik => (
            <>
              <h1>{props.record["__typename"]}</h1>
              {displayFields(props).map(field => (
                <div key={field} style={{ marginBottom: "20px" }}>
                  <h4>{field}</h4>
                  <div>
                    <FieldInput name={field} value={props.record[field]} />
                  </div>
                </div>
              ))}
            </>
          )}
        </Formik>
      </Box>
    </Drawer>
  )
}
