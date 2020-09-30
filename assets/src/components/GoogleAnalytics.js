import React from 'react';
import { withRouter } from 'react-router-dom';
import { GA_TRACKING_ID } from '../../config.js';

class GoogleAnalytics extends React.Component {

    // track the initial page view of the home page
    componentDidMount() {
        gtag('config',  "UA-178080782-1", {  //GA_TRACKING_ID, {
            'page_title': document.title,
            'page_location': window.location.href,
            'page_path': location.pathname
        });
        console.log("tracking initial page view of home page")
    }

    componentWillUpdate ({ location, history }) {
        const gtag = window.gtag;

        console.log("location.pathname = " + location.pathname);
        console.log("this.props.location.pathname = " + this.props.location.pathname)
        console.log("typeof(gtag) = " + typeof(gtag))

        if (location.pathname === this.props.location.pathname) {
            // don't log identical link clicks (nav links likely)
            console.log("identical page!")
            return;
        }

        if (history.action === 'PUSH' &&
            typeof(gtag) === 'function') {
            gtag('config',  "UA-178080782-1", {  //GA_TRACKING_ID, {
                'page_title': document.title,
                'page_location': window.location.href,
                'page_path': location.pathname
            });
        }
    }

    render () {
        return null;
    }
}

export default withRouter(GoogleAnalytics);
