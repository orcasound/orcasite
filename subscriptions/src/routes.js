var _subscriptionService = require('./subscriptionService');

function subscribeHandler(request, h) {
    var payload = request.payload;
    var result = _subscriptionService.subscribe(payload);
    if (result) {
        resp = h.response('Created').code(201);
        return resp;    
    }
    
    resp = h.response('OK').code(200);
    return resp;
}
    
function unsubscribeHandler(request, h) {
    var payload = request.payload;
    var result = _subscriptionService.unsubscribe(payload);
    if (result) {
        return payload;
    }
    var resp = h.response('Bad Request').code(400);
    return resp;
}

function notifyHandler(request, h) {
    try {
        _subscriptionService.notify(request.payload);
    }
    catch (e) {
        if (e)
            return h.response(e.message).code(400);
    }
    return h.response('OK').code(200);
}

module.exports = {
    subscribeHandler: subscribeHandler,
    unsubscribeHandler: unsubscribeHandler,
    notifyHandler: notifyHandler
}