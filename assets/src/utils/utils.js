export const formatTimestamp = timestamp => {
  const date = new Date(timestamp)

  const options = {
    year: "2-digit",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit"
  }

  return date.toLocaleString(options)
}
