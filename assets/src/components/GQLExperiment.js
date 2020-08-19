import React, {Component} from 'react'
import {Query} from 'react-apollo'

import {string, func} from 'prop-types'

import {GET_FEED} from 'queries/feeds'

import Loader from 'components/Loader'

export default class GQLExperiment extends Component {

render() {
    const slug = "orcasound-lab"

    return (
        <Query query={GET_FEED} variables={{slug}}>
            {({data, loading, error}) => {
                if (!loading) {

                    const {feed} = data
                    const {introHtml} = feed

                    return (
                    <div className="feed-page mb-4">
                        {introHtml && <div dangerouslySetInnerHTML={{__html: introHtml}} />}
                    </div>
                    )
                } else {
                    return ""
                }
            }}
        </Query>
    )
}
    

}


