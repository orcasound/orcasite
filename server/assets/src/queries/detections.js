import gql from "graphql-tag"

export const LIST_ALL_DETECTIONS = gql`
  {
    detectionsAll {
      id
      playlistTimestamp
      playerOffset
      timestamp
      listenerCount
      description
      feed {
        id
        name
        slug
      }
    }
  }
`

export const LIST_DETECTIONS = gql`
  query getDetections($pagination: Pagination!) {
    detections(pagination: $pagination) {
      meta {
        currentPage
        previousPage
        nextPage
        totalEntries
        totalPages
      }
      entries {
        id
        playlistTimestamp
        playerOffset
        timestamp
        listenerCount
        description
        feed {
          id
          name
          slug
        }
      }
    }
  }
`

export const LIST_CANDIDATES = gql`
  query getCandidates($pagination: Pagination!) {
    candidates(pagination: $pagination) {
      meta {
        currentPage
        previousPage
        nextPage
        totalEntries
        totalPages
      }
      entries {
        id
        minTime
        maxTime
        detectionCount
        detections {
          id
          timestamp
          playlistTimestamp
          playerOffset
          listenerCount
          description
        }
        feed {
          id
          name
          slug
          nodeName
        }
      }
    }
  }
`
