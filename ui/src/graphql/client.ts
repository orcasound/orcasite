/* eslint-disable import/no-unused-modules */
export const endpointUrl =
  process.env.API_ENDPOINT ?? 'http://localhost:4000/graphql'

export const fetchParams = {
  headers: {
    'Content-Type': 'application/json; charset=utf-8',
  },
}
