export const AUTH_TOKEN = "orcasound:auth_token"

export const getToken = () => {
  return localStorage.getItem(AUTH_TOKEN)
}

export const logIn = auth_token => {
  return localStorage.setItem(AUTH_TOKEN, auth_token)
}

export const logOut = () => {
  return localStorage.removeItem(AUTH_TOKEN)
}
