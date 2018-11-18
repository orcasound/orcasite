'use strict';

const Hapi=require('hapi');
const request=require('request');
const Async=require('async');

var routeOptions = {
    payload:{ 
        allow: ['application/json']
    }
};
const subscriptions = [];

// Create a server with a host and port
const server=Hapi.server({
    host:'localhost',
    port:8000
});

// Subscribe route
server.route({
    method:'POST',
    path:'/subscribe',
    handler: subscribeHandler
});

function subscribeHandler(request, h) {
    var payload = request.payload;
    var resp;
    var findDup = subscriptions.find((subscription) => {
        return subscription.host == payload.host;
    });
    console.log(subscriptions);

    if (findDup === undefined){
        subscriptions.push(payload);
        resp = h.response('created').code(201);
        return resp;    
    }
    
    resp = h.response('ok').code(200);
    return resp;
}


// Unsubcribe
server.route({
    method:'POST',
    path:'/unsubscribe',
    handler:unsubscribeHandler
});

function unsubscribeHandler(request, h) {
    var payload = request.payload;
    
    var index = subscriptions.findIndex((subscription) => {
        return subscription.host == payload.host;
    });

    if (index != -1) {
        subscriptions.splice(index);
        return payload;
    }
    else {
        var resp = h.response('Invalid Subscription').code(400);
        return resp;
    }
}

// Notify
server.route({
    method:'POST',
    path:'/notify',
    options: routeOptions, 
    handler: notifyHandler
});

function notifyHandler(request, h) {
    Async.each(subscriptions, (subscription, cb) => {
        request.post(subscription.host, (err) => {
            cb();
        });
    }, (err) => {
        console.log(err);
    });
    return h.response('ok').code(200);
}


// Start the server
async function start() {

    try {
        await server.start();
    }
    catch (err) {
        console.log(err);
        process.exit(1);
    }

    console.log('Server running at:', server.info.uri);
};


start();