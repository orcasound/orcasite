'use strict';
const hapi = require('hapi');
const routes = require('./routes.js');

var routeOptions = {
    payload:{ 
        allow: ['application/json']
    }
};

// Create a server with a host and port
const server=hapi.server({
    host:'localhost',
    port:8000
});

// Subscribe route
server.route({
    method:'POST',
    path:'/subscribe',
    handler: routes.subscribeHandler,
    options: routeOptions
});

// Unsubcribe
server.route({
    method:'POST',
    path:'/unsubscribe',
    handler: routes.unsubscribeHandler,
    options: routeOptions
});

// Notify
server.route({
    method:'POST',
    path:'/notify',
    handler: routes.notifyHandler,
    options: routeOptions
});

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