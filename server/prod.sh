#!/bin/sh
export MIX_ENV=prod

if [ -n "$IS_REVIEW_APP" ]; then
  echo "Heroku review app detected: ${HEROKU_APP_NAME}"
  export HOST_URL="${HEROKU_APP_NAME}.herokuapp.com"
  echo "HOST_URL: ${HOST_URL}"
  export URLS="//${HOST_URL}"
  echo "URLS: ${URLS}"
fi

export API_ENDPOINT="https://${HOST_URL}/graphql"
echo "Using API endpoint: ${API_ENDPOINT}"

echo "Starting server"
mix phx.server