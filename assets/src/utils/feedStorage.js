export const storeCurrentFeed = currentFeed => {
  localStorage.setItem('currentFeed', JSON.stringify(currentFeed))
}

export const getCurrentFeed = () => {
  try {
    return JSON.parse(localStorage.getItem('currentFeed'))
  } catch(error) {
    localStorage.removeItem('currentFeed')
  }
}

