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

export const LIST_CANDIDATES = gql`
  {
    candidates {
      minTime
      maxTime
      detectionCount
      detections {
        id
        timestamp
        playlistTimestamp
        playerOffset
        listenerCount
      }
      feed {
        id
        name
        slug
      }
    }
  }
`
