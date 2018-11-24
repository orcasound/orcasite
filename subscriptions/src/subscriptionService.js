const async = require('async');
var _r2 = require('r2');
var _subscriptions = [];

function r2(r2) {
    _r2 = r2;
}

function subscribe(subscription) {
    console.log('Subscribing: ' + subscription.host);    
    var findDup = _subscriptions.find((currentSubscription) => {
        return currentSubscription.host == subscription.host;
    });

    if (findDup === undefined){
        _subscriptions.push(subscription);
        return true;    
    }
    return false;
}
    
function unsubscribe(subscription) {
    console.log('Unsubscribing: ' + subscription.host);
    var index = _subscriptions.findIndex((currentSubscription) => {
        return currentSubscription.host === subscription.host;
    });

    if (index != -1) {
        _subscriptions.splice(index);
        return true;
    }
    else {
        return false;
    }
}

function notify(notification) {
    if (notification.event === undefined) {
        throw new Error('"event" field must be present');         
    }

    if (notification.event.type === undefined) {
        throw new Error('"type" field must be present');   
    }

    async.each(_subscriptions, async (subscription) => {
        try {
            console.log('Notifying: ', subscription.host);
            var response = await _r2.post(subscription.host, {json:notification}).response;
            console.log("Response: ", await response.json());
        }
        catch (e) { 
            console.log('Error: ' + e);
            throw e;
        };
    }, (err) => {
        if (err != undefined)
            console.log(err);
    });
}

function getSubscriptions() {
    return _subscriptions.slice(0);
}

module.exports = {
    subscribe: subscribe,
    unsubscribe: unsubscribe,
    notify: notify,
    getSubscriptions: getSubscriptions,
    r2: r2
}