import React, { Component } from 'react'
import { Query } from 'react-apollo'
import { string, func } from 'prop-types'
import { GET_FEED } from '../queries/feeds'
import Loader from './Loader'
import { Typography } from '@material-ui/core'
import styled from "styled-components"

const FeedPageContainer = styled.div`
  img {
    z-index: 0;
  }
`
const FeedName = styled(Typography)`
  font-size: 20px;
  font-weight: 400;
  color: rgba(0,0,0,0.87);
  letter-spacing: 0;
  text-align: left;
  line-height: 28px;
  padding: 1rem 0rem 1rem 2rem;
`

const FeedImageContainer = styled.div`
  height: 50%;
  background-image: url("https://s3.us-west-2.amazonaws.com/orcasite/rpi_orcasound_lab/map.png?response-content-disposition=inline&X-Amz-Security-Token=AgoGb3JpZ2luEKv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLXdlc3QtMiKAAgKQKUJ8j2zMJMYfAg4f%2BAYvTVq%2FtftSDZjZSRhdQLNIlLgB7MQzCElnx4NGdiJ7eB3gnNZS7TnxmhY264TQTcdHWAPd6t5yFKd8va4P8ZrzhmsMUbUqQtBXidCjs1MycNGt4proco8Vn5Z6ysRRtbO%2FNIi10bslI9epLoUXwORKaG24XDx%2BdbGQ6S7816VHT4CwoTnzMAd31ogY2Op%2FwUT9hGzgFOAiRck53UNgd%2BAysXoCSBEFk44RLd%2FK1Dw4ULquq66XcddmU7HX24x4%2FAXVwYCvFtNcHPdzMRehqcy1fNYJD5%2FcMunZ7p2nm3%2BxwxS7BNI%2FpR4qnDFEZWJHfjYq%2BwMIgP%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FARAAGgw2NDgxODM3MjQ1NTUiDCq7B1%2FAPgq2l4RWbSrPA9NF0NKI1pFV5%2BTEvdPiazCrpep5s%2FN8lfPnvlcmG2Rvf25K9aZy9wvaLcUx9JPLkfazVWemOpMIi8sAp%2F4YDYPj31xSGzb0hxX9D88Sw2Vailnrs10%2BE91A5%2FaEbdjC%2Fp60vQXhtYZlWtzZ23nT1mzqOSBVUZ1B1zKkg3%2BGCzU0ipKRxW2djIFR8hbWTRfOQsHAzxEMirD3MTy56iTrEOEvAFEPCqomSyxNZJcY6mxHv1VLfFzty71hgyzehL94%2B3ofz1o5jezE3ldHtsmE2hgEuDdJAMr%2FRfVHoUATbuVn5xwuhRBNNYmh2hndR7wQZ7lVzvFUqYLglBi5ZVD0ZbF3SufGVpECgrSTWki4f6YzAWbKc17HzklgGZngZd5gX3UofJEmgG%2Fko4hY36yRFWaPqCaRJzJaICKvbAtuB36c%2B1blnnMCkprnICuvvvt3GM8DQk2W19i1K1FXjn3kY5C841GJ4XlHTDOqE3D87sOmgqVWM8mkrk1xhy8r8g8e5xXAgYVRLeXcO%2BEYWvDYTZJq5Koq2hdxPzaBPwB%2BNeedsyXotvazgjw7E2AVKQHf%2BesxL6fWe7KKpzf%2BpZZx1H5fYpDSpXqdoFH2%2BWjDvxMwgab25wU%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20190609T234408Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAZN2WCXIFQCB3WGZN%2F20190609%2Fus-west-2%2Fs3%2Faws4_request&X-Amz-Signature=f20d6faa1a550f0e3eb31e5621afe9a29cdb61ac0978dcd70cf0261bbed3caa2");
  background-size: 375px;
  height: 127px;
  background-color: gray;
`

const StyledIntroHTML = styled.div`
  padding: 1rem 3rem 1rem 2rem;
`

class FeedPageV2 extends Component {
  state = { show: false }

  static propTypes = {
    feedSlug: string.isRequired,
    onChangeFeed: func,
  }

  showModal = () => {
    this.setState({ show: true })
  }

  hideModal = () => {
    this.setState({ show: false })
  }

  render() {
    const { feedSlug: slug } = this.props

    return (
      <Query query={GET_FEED} variables={{ slug }}>
        {({ data, loading, error }) => {
          if (loading) {
            return <Loader />
          }

          const { feed } = data

          if (error || !feed) {
            return <div>Feed Not Found for {slug}</div>
          }


          const { introHtml, thumbUrl } = feed

          return (
            <FeedPageContainer>
              <FeedName variant="h2">
                {feed.name}
              </FeedName>
              <FeedImageContainer>

              </FeedImageContainer>
              {this.props.children}
              <StyledIntroHTML>
                {introHtml && <div dangerouslySetInnerHTML={{ __html: introHtml }} />}
              </StyledIntroHTML>
            </FeedPageContainer>
          )
        }}
      </Query>
    )
  }
}

export default FeedPageV2
