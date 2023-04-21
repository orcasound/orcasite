import { GraphQLClient } from 'graphql-request'

import { getSdk } from '../generated/types'

// TODO: get API URL from env
const API_ENDPOINT_STATIC = 'http://live.orcasound.net/graphql'

const apiClient = getSdk(new GraphQLClient(API_ENDPOINT_STATIC))

export default apiClient
