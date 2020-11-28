import React from 'react'
import {render} from 'react-dom'

import {ApolloProvider} from 'react-apollo'
import apolloClient from './apolloClient'

import Root from './components/Root'

render(
  <ApolloProvider client={apolloClient}>
    <Root />
  </ApolloProvider>,
  document.getElementById('root'),
);            


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
