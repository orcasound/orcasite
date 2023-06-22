echo

# Configure env vars dynamically for review apps
if [ -n "$IS_REVIEW_APP" ]; then
  echo "Heroku review app detected: ${HEROKU_APP_NAME}"
  export HOST_URL="${HEROKU_APP_NAME}.herokuapp.com"
  echo "HOST_URL: ${HOST_URL}"
  export URLS="//${HOST_URL}"
  echo "URLS: ${URLS}"
fi

export API_ENDPOINT=$HOST_URL
echo "Using API endpoint: ${API_ENDPOINT}"