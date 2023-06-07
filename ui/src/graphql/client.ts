/* eslint-disable import/no-unused-modules */

// TODO: move heroku review app specific stuff out of here
export const endpointUrl = process.env.IS_REVIEW_APP
  ? `https://${process.env.HEROKU_APP_NAME}.herokuapp.com/graphql`
  : process.env.API_ENDPOINT ?? 'http://localhost:4000/graphql'

export const fetchParams = {
  headers: {
    'Content-Type': 'application/json; charset=utf-8',
  },
}
