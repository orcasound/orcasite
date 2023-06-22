/* eslint-disable import/no-unused-modules */
if (!process.env.NEXT_PUBLIC_API_ENDPOINT) {
  throw new Error('NEXT_PUBLIC_API_ENDPOINT is not set')
}

const protocol = process.env.NODE_ENV === 'production' ? 'https' : 'http'

export const endpointUrl = `${protocol}://${process.env.NEXT_PUBLIC_API_ENDPOINT}/graphql`

export const fetchParams = {
  headers: {
    'Content-Type': 'application/json; charset=utf-8',
  },
}
