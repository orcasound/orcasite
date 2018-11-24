# Subscriptions module

## Overview
The subscriptions module is a micro service which provides a pub/sub mechanism for sending notificatiopns via webhook invocations.

## Installing
Clone the repo and then run `npm install` on the folder to install all the modules.

## Running the service
`npm start` will start the service listening on localhost:8000

## Unit tests
`npm test` will run the supplied unit tests.

## HTTP API
The subscription module provides the following API.

### /subscribe ###
Subscribes to receive notifications. 

#### Content:

{"host": "{url}"}

#### Returns ####

201 - If the subscription was added
200 - If the subscription already existed

### /unsubscribe ###
Removes a subscription.

#### Content:

{"host":"url"}

#### Returns

200 - Subscription was successfully removed
400 - Subscription did not exist

### /notify ###
Sends a POST request to all subscribers. The content for the POST is whatever content has been posted to the notify API. This method is fire and forget. 

#### Content:
A JSON "event" object.

{
    "event": {
        "type": "some event"
    }
}

#### Returns
200 - Content is valid, all subscribers will be notified
400 - Content is invalid (missing event / type). 

#### Live test
Running `test.sh` will subscribe and fire 2 notifications against a live webhook endpoint hosted on the webtask.io Serverless platform

The output will look like the following.

```
Server running at: http://localhost:8000
Subscribing: https://wt-glenn-block-gmail-com-0.sandbox.auth0-extend.co
m/orca?instance=1
Subscribing: https://wt-glenn-block-gmail-com-0.sandbox.auth0-extend.co
m/orca?instance=2
Notifying:  https://wt-glenn-block-gmail-com-0.sandbox.auth0-extend.com
/orca?instance=1
Notifying:  https://wt-glenn-block-gmail-com-0.sandbox.auth0-extend.com/orca?instance=2
Response:  { invoked: '/orca?instance=1' }
Response:  { invoked: '/orca?instance=2' }
```



## TODO
* - Add better logging
* - Add persisting subscriptions to the DB.
* - Add API key auth
* - Add filtering for subscriptions (subscribe to a subset of events)






