import { GraphQLClient } from 'graphql-request'

// TODO: get API URL from env
// const API_ENDPOINT_STATIC = 'https://live.orcasound.net/graphql'
const API_ENDPOINT_STATIC = 'http://localhost:4000/graphql'

const apiClient = new GraphQLClient(API_ENDPOINT_STATIC)

export default apiClient
