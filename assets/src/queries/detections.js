import gql from 'graphql-tag'

export const LIST_DETECTIONS = gql`
{
  detections {
    id
    playlistTimestamp
    playerOffset
    timestamp
    feed {
      id
      name
      slug
    }
  }
}
`
