import { GraphQLClient } from 'graphql-request'

// TODO: get API URL from env
const API_ENDPOINT_STATIC = 'https://live.orcasound.net/graphql'

const apiClient = new GraphQLClient(API_ENDPOINT_STATIC)

export default apiClient
