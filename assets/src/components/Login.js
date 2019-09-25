import React, { Component } from "react"
import { Link } from "react-router-dom"
import Humanize from "humanize-plus"
import { compose } from "recompose"
import { graphql } from "@apollo/react-hoc"
import gql from "graphql-tag"
import { withFormik, Form, Field } from "formik"
import { object, string } from "yup"
import CircularProgress from "@material-ui/core/CircularProgress"
import ChevronRightIcon from "@material-ui/icons/ChevronRight"

import { logIn } from "utils/auth"

import { StylesProvider, ThemeProvider } from "@material-ui/styles"
import CssBaseline from "@material-ui/core/CssBaseline"
import theme from "./theme"

import Header from "./SiteMenu"

import classNames from "classnames"

import "styles/login.scss"

class Login extends Component {
  submitErrorMessage = ({ submitErrors }) => (
    <div className="alert alert-danger">
      <h4 className="alert-heading">Uh oh</h4>
      <p>
        We ran into {submitErrors.length > 1 ? "a few issues" : "an issue"}{" "}
        while sending your information:
      </p>
      <ul>
        {submitErrors.map(message => (
          <li key={message}>{message}</li>
        ))}
      </ul>
      <hr />
      <p>
        It wasn't actually supposed to happen. Can you please let us know by
        emailing us at{" "}
        <a href="mailto:admin@orcasound.net">admin@orcasound.net</a>? We'll get
        it taken care of for you.
      </p>
    </div>
  )

  submitButton = isSubmitting => (
    <button
      type="submit"
      className="btn btn-lg btn-primary w-100 d-flex align-items-center justify-content-center"
      disabled={isSubmitting}
    >
      {isSubmitting ? "Sending..." : this.props.login ? "Login" : "Register"}
      {isSubmitting && (
        <span className="ml-2">
          <CircularProgress size={25} color={"inherit"} />
        </span>
      )}
      {!isSubmitting && <ChevronRightIcon />}
    </button>
  )

  render() {
    const props = this.props
    const {
      values,
      errors,
      touched,
      isSubmitting,
      handleChange,
      handleBlur,
      handleSubmit
    } = props

    return (
      <StylesProvider injectFirst>
        <ThemeProvider theme={theme}>
          <CssBaseline />
          <div className="login">
            <Header />
            <Form className="login-form">
              <h4 className="my-5 text-center">
                {(props.login && "Log in") || "Register an account"}
              </h4>
              {errors.submitErrors && this.submitErrorMessage(errors)}
              <div className="d-flex flex-column">
                {!props.login && (
                  <Field
                    name="firstName"
                    placeholder="First name"
                    className="form-control form-control-lg mb-3"
                  />
                )}

                {!props.login && (
                  <Field
                    name="lastName"
                    placeholder="Last name"
                    className="form-control form-control-lg mb-3"
                  />
                )}

                <Field
                  name="email"
                  placeholder="Email"
                  type="email"
                  className={`form-control form-control-lg ${(((errors.email &&
                    touched.email) ||
                    props.errors.error) &&
                    "is-invalid") ||
                    "mb-3"}`}
                />
                {errors.email && touched.email && (
                  <div className="invalid-feedback mb-3">{errors.email}</div>
                )}

                {props.errors.error && (
                  <div className="invalid-feedback mb-3">
                    {props.errors.error.email}
                  </div>
                )}

                <Field
                  name="password"
                  type="password"
                  placeholder="Password"
                  className={`form-control form-control-lg ${(errors.password &&
                    touched.password &&
                    "is-invalid") ||
                    "mb-3"}`}
                />
                {errors.password && touched.password && (
                  <div className="invalid-feedback mb-3">{errors.password}</div>
                )}

                <div className="d-flex mt-2 flex-column justify-content-center align-items-center">
                  {this.submitButton(isSubmitting)}

                  {props.login ? (
                    <Link to="/register" className="mt-5">
                      Need an account?
                    </Link>
                  ) : (
                    <Link to="/login" className="mt-5">
                      Already have an account?
                    </Link>
                  )}
                </div>
              </div>
            </Form>
          </div>
        </ThemeProvider>
      </StylesProvider>
    )
  }
}

const SIGNUP_MUTATION = gql`
  mutation SignupMutation(
    $email: String!
    $password: String!
    $firstName: String!
    $lastName: String!
  ) {
    signup(
      email: $email
      password: $password
      firstName: $firstName
      lastName: $lastName
    ) {
      authToken
    }
  }
`

const LOGIN_MUTATION = gql`
  mutation LoginMutation($email: String!, $password: String!) {
    login(email: $email, password: $password) {
      id
      authToken
    }
  }
`

export default compose(
  graphql(LOGIN_MUTATION, { name: "loginMutation" }),
  graphql(SIGNUP_MUTATION, { name: "signupMutation" }),
  withFormik({
    mapPropsToValues: props => ({
      firstName: "",
      lastName: "",
      email: "",
      password: ""
    }),
    validationSchema: object().shape({
      email: string()
        .email(
          "This doesn't seem like an email (needs to look like name@example.com)"
        )
        .required("Please enter your email."),
      password: string()
        .min(6, "Password should be at least ${min} characters long.")
        .required("Please enter a password!")
    }),
    handleSubmit: async (
      values,
      { props, setSubmitting, setErrors, setFieldError }
    ) => {
      try {
        console.log("submitting", props)
        const { firstName, lastName, email, password } = values
        if (props.login) {
          const result = await props.loginMutation({
            variables: {
              email,
              password
            }
          })

          setSubmitting(false)
          const { authToken } = result.data.login
          logIn(authToken)
        } else {
          const result = await props.signupMutation({
            variables: {
              firstName,
              lastName,
              email,
              password
            }
          })
          setSubmitting(false)
          const { authToken } = result.data.signup
          logIn(authToken)
        }

        if (props.location.state && props.location.state.from) {
          props.history.push(props.location.state.from)
        } else {
          props.history.push(`/`)
        }
      } catch (error) {
        setSubmitting(false)
        const { graphQLErrors } = error
        const errors = graphQLErrors || []
        errors.forEach(({ message, errors }) => {
          if (message === "validation") {
            Object.keys(errors).forEach(key => {
              setFieldError(
                key,
                `${Humanize.capitalize(key)} ${errors[key][0]}`
              )
            })
          }
          if (message === "wrong_credentials")
            setFieldError("email", "Your email or password were invalid.")
        })
      }
    },
    displayName: "LoginForm"
  })
)(Login)
