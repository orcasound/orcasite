curl http://localhost:8000/subscribe -H "content-type: application/json" -d '{"host":"https://wt-glenn-block-gmail-com-0.sandbox.auth0-extend.com/orca?instance=1"}'
echo
curl http://localhost:8000/subscribe -H "content-type: application/json" -d '{"host":"https://wt-glenn-block-gmail-com-0.sandbox.auth0-extend.com/orca?instance=2"}'
echo
curl http://localhost:8000/notify -H "content-type: application/json" -d '{"event":{"type":"WHALE ALERT"}}'
echo