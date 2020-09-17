import React from 'react'
import {render} from 'react-dom'

import {ApolloProvider} from 'react-apollo'
import apolloClient from './apolloClient'

import Root from './components/Root'
import ReactGA from 'react-ga'

import { GA_TRACKING_ID } from '../config.js';


//if (ENV.GOOGLE_ANALYTICS_ID) {
//  ReactGA.initialize(ENV.GOOGLE_ANALYTICS_ID);
  //ReactGA.pageview(window.location.pathname + window.location.search);
  //ReactGA.modalview("/");
  //console.log(window.localtion.pathname + window.location.search);
  //ReactGA.modalview('/bush-point');
  //ReactGA.modalview('/port-townsend');
  //ReactGA.modalview('/orcasound-lab');
  //ReactGA.modalview('/paul-test');
  //ReactGA.modalview('/steve-test');
//}

//<script async src="https://www.googletagmanager.com/gtag/js?id=UA-178080782-1"></script>


if (true) {

  var Promise = require('promise')
  var https = require('https')
  var url = require('url')
  var vm = require('vm')
  var concat = require('concat-stream'); // this is just a helper to receive the
                                        // http payload in a single callback
                                        // see https://www.npmjs.com/package/concat-stream

  const requestUrl = url.parse(url.format({
    protocol: 'https',
    hostname: 'www.googletagmanager.com',
    pathname: '/gtag/js',
    query: {
      id: 'UA-178080782-1'
    }
  }));

  var gtagInitializationSuccessCallback = () => {
    console.log("Executed google tag manager code!");
    render(
      <ApolloProvider client={apolloClient}>
        <Root />
      </ApolloProvider>,
      document.getElementById('root'),
    )            
  }

  var gtagInitializationFailureCallback = () => {
    console.log("oops, somthing went wrong....")
  }

 
  const req = https.get(
    {
      hostname: requestUrl.hostname,
      path: requestUrl.path,
    }, (response) => {
      console.log("Received response from the googletagmanager server!");
      response.pipe(concat({ encoding: 'string' }, function(remoteSrc) {        
       
        var myPromise = new Promise(function(resolve, reject){
          vm.runInThisContext(remoteSrc);
          resolve("promise resolved...");
        });

        myPromise.then(gtagInitializationSuccessCallback, gtagInitializationFailureCallback);

      }));  
  });

}

/*

render(
  <ApolloProvider client={apolloClient}>
    <Root />
  </ApolloProvider>,
  document.getElementById('root'),
)            

*/


/*
http.get({
    host: 'https://www.googletagmanager.com/gtag/js?id=UA-178080782-1', 
    //port: 80, 
    //path: '/gtag/js?id=UA-178080782-1'
  }, 
  function(res) {
    res.setEncoding('utf8');
    res.pipe(concat({ encoding: 'string' }, function(remoteSrc) {
      //vm.runInThisContext(remoteSrc, 'remote_modules/hello.js');
      vm.runInThisContext(remoteSrc);
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', "UA-178080782-1");
      console.log("Feteched and initialized GA global tag successfully!");
    }));
});
*/

/*
render(
  <ApolloProvider client={apolloClient}>
    <Root />
  </ApolloProvider>,
  document.getElementById('root'),
)
*/
