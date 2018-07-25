import {shape, string} from 'prop-types'
export const feedType = shape({
  name: string,
  slug: string,
  node: string,
})
