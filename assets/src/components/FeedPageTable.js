import React from "react"

import {GetAppOutlined as DownloadIcon, ShareOutlined as ShareIcon, PlayCircleOutline as PlayCircleOutlineIcon, ThumbUpOutlined as ThumbUpIcon, Alarm as AlarmIcon, Delete as DeleteIcon, AddShoppingCart as AddShoppingCartIcon} from "@material-ui/icons"
import {makeStyles, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, IconButton, Paper, Box, ThemeProvider} from "@material-ui/core"

import { createMuiTheme, makeStyle} from '@material-ui/core/styles';
import purple from '@material-ui/core/colors/purple';

import OrcaIcon from "./Asset 5.svg"
import ShipIcon from "./Asset 8.svg"
import FishIcon from "./Asset 4.svg"
import SoundIcon from "./Asset 3.svg"

import TableEntry from "./TableEntry"

import { Query } from "react-apollo"

import Loader from "./Loader"

import { LIST_DETECTIONS } from "../queries/detections"
import { LIST_DETECTIONS_FROM_FEED } from "../queries/detections"



function createData(time, eventTypeStr, imageSrc) {
  return { time, eventTypeStr, imageSrc };
}

const rows = [
  createData('9:00 AM August 13 2020', "orca", OrcaIcon),
  createData('1:27 PM August 02 2020', "ship", ShipIcon),
  createData('6:29 AM July 30 2020', "fish", FishIcon),
  createData('6:25 AM July 30 2020', "mystery sound", SoundIcon),
];

class FeedPageTable extends React.Component {

  constructor(props) {
    super(props)
    this.state = {detections: null, feed: this.props.feed};
    console.log("here")
  }

  componentDidMount() {

  }

  render() {

    debugger;


    return (
      <div style={{display: "flex", flexDirection: "column"}}>
          
        <div style={{display: "flex", flexDirection: "row", backgroundColor: "#E5E5E5"}}>
          <div style={{width:"100%", textAlign:"left", width:"25%"}}>Time</div>
          <div style={{width:"100%", textAlign:"left", width:"37.5%"}}>Sound</div>
          <div style={{width:"100%", textAlign:"left", width:"37.5%"}}>Action</div>
        </div>

        <div style={{display: "block", overflow: "scroll", height: "200px"}}>

          <Query query={LIST_DETECTIONS} >
            {({data, loading, error}) => {
                if (loading) {
                  return <Loader/>
                }

                if (error || !data) {
                  return <div>Error detections data not available</div>
                }

                // at the moment, the most recent detection is at the end of the detections array

                const detections = data.detections;
                return (
                  <>
                    {detections
                      .reverse()
                      .map(detection => {
                        if (detection.feed.id == this.state.feed.id) { 
                          return <TableEntry key={detection.id} type={detection.type} dateTime={detection.timestamp} detection={detection} nodeName={this.state.feed.nodeName}/>
                        }
                      })
                    }
                  </>
                )
              }}
          </Query>

         {/*<TableEntry type="ship" dateTime="8:44 PM August 02 2020" />
         <TableEntry type="fish" dateTime="1:29 PM August 02 2020"/>
         <TableEntry type="mystery" dateTime="6:25 AM July 30 2020"/>
         <TableEntry type="mystery" dateTime="6:25 AM July 30 2020"/>
         <TableEntry type="mystery" dateTime="6:25 AM July 30 2020"/>
         <TableEntry type="mystery" dateTime="6:25 AM July 30 2020"/>
         <TableEntry type="mystery" dateTime="6:25 AM July 30 2020"/>*/}

        </div>

      </div>
    );
  }
}



export default  FeedPageTable