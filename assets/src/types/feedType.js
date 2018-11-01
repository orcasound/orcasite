import {shape, string, object} from 'prop-types'
export const feedType = shape({
  name: string.isRequired,
  slug: string.isRequired,
  nodeName: string.isRequired,
  locationPoint: object,
  introHtml: string
})
