import React from 'react';
import { withRouter } from 'react-router-dom';

class GoogleAnalytics extends React.Component {

    // track the initial page view
    componentDidMount() {
        console.log("Google Analytics ID = " + ENV.GOOGLE_ANALYTICS_ID);

        gtag('config', `${ENV.GOOGLE_ANALYTICS_ID}`, {
            'page_title': document.title,
            'page_location': window.location.href,
            'page_path': location.pathname
        });
    }

    UNSAFE_componentWillUpdate ({ location, history }) {
        const gtag = window.gtag;

        if (location.pathname === this.props.location.pathname) {
            return;
        }

        if (history.action === 'PUSH' &&
            typeof(gtag) === 'function') {
            gtag('config', `${ENV.GOOGLE_ANALYTICS_ID}`, {
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
