/* eslint-disable import/no-unused-modules */
if (!process.env.NEXT_PUBLIC_API_ENDPOINT) {
  throw new Error('NEXT_PUBLIC_API_ENDPOINT is not defined')
}

export const endpointUrl = process.env.NEXT_PUBLIC_API_ENDPOINT

export const fetchParams = {
  headers: {
    'Content-Type': 'application/json; charset=utf-8',
  },
}
