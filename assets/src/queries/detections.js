import gql from "graphql-tag"

export const LIST_DETECTIONS = gql`
  {
    detections {
      id
      playlistTimestamp
      playerOffset
      timestamp
      listenerCount
      feed {
        id
        name
        slug
      }
    }
  }
`

export const LIST_DETECTION_GROUPS = gql`
  {
    detectionGroups {
      timeBucket
      detections
      feed
    }
  }
`
