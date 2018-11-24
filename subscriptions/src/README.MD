# Subscriptions module

## Overview
The subscriptions module is a micro service which provides a pub/sub mechanism for sending notificatiopns via webhook invocations.

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

#### Acceptance test


## TODO
* - Add better logging
* - Add persisting subscriptions to the DB.
* - Add API key auth
* - Add filtering for subscriptions (subscribe to a subset of events)






