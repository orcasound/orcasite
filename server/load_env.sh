echo

# Configure env vars dynamically for review apps
if [ -n "$IS_REVIEW_APP" ]; then
  echo "Heroku review app detected: ${HEROKU_APP_NAME}"
  export HOST_URL="${HEROKU_APP_NAME}.herokuapp.com"
  echo "HOST_URL: ${HOST_URL}"
  export URLS="//${HOST_URL}"
  echo "URLS: ${URLS}"
fi

export GQL_ENDPOINT="https://${HOST_URL}/graphql"
export SOCKET_ENDPOINT="wss://${HOST_URL}/socket"
echo "Using GQL endpoint: ${GQL_ENDPOINT}"
echo "Using socket endpoint: ${SOCKET_ENDPOINT}"