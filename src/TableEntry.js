import React from "react"

//import {GetAppOutlined as DownloadIcon, ShareOutlined as ShareIcon, PlayCircleOutline as PlayCircleOutlineIcon, ThumbUpOutlined as ThumbUpIcon, Alarm as AlarmIcon, Delete as DeleteIcon, AddShoppingCart as AddShoppingCartIcon} from "@material-ui/icons"
import {makeStyles, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, IconButton, Paper, Box, ThemeProvider} from "@material-ui/core"

import { createMuiTheme, makeStyle} from '@material-ui/core/styles';
import purple from '@material-ui/core/colors/purple';

import OrcaIcon from "./Asset 5.svg"
import ShipIcon from "./Asset 8.svg"
import FishIcon from "./Asset 4.svg"
import SoundIcon from "./Asset 3.svg"

import { SvgIcon } from '@material-ui/core';

import LikeIcon from "./icons/LikeIcon.js"
import PlayIcon from "./icons/PlayIcon.js"
import ShareIcon from "./icons/ShareIcon"
import DownloadIcon from "./icons/DownloadIcon"

//import {ReactComponent as homemadeDownloadIcon} from "./download-icon.svg"
//import {ReactComponent as homemadeShareIcon} from "./share-icon.svg"
//import {ReactComponent as homemadePlayIcon} from "./play-icon.svg"

//import homemadePlayIcon from "./play-icon.svg"

class TableEntry extends React.Component {
    
    constructor(props) {
      super(props);
      this.state = {type: this.props.type, dateTime: this.props.dateTime};
    }

    componentDidMount() {
    }

    render() {
        return(
          <>
            <div style={{display:"flex", width:"100%", height: "64px", flexDirection: "row", marginBottom:"15px", marginTop:"15px", paddingTop:"8px"}} >
            <div style={{wordWrap: "break-word", width:"25%"}}>{this.state.dateTime}</div>
            <div className="record" style={{display: "flex", flexDirection: "row", backgroundColor: "#E9E9E9", borderRadius:"3px", width:"75%", alignItems: "center", alignContent: "center"}}>             
              <div style={{textAlign:"left", width:"100%"}}>
                  A user heard an {this.state.type}
              </div>
              <div style={{textAlign:"left", width:"100%"}}>
                <IconButton onClick={this.playIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <PlayIcon />{/* <SvgIcon component={homemadePlayIcon} /> */} {/*<PlayCircleOutlineIcon style={{color: "#346390"}} />*/}
                </IconButton>
                <IconButton onClick={this.thumbUpIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <LikeIcon /> {/* <SvgIcon component={homemadeLikeIcon} /> */} {/*<ThumbUpIcon /> */} 
                </IconButton>      
                <IconButton onClick={this.shareIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <ShareIcon /> {/*<SvgIcon component={homemadeShareIcon} /> */} {/*<ShareIcon />*/}
                </IconButton>      
                <IconButton onClick={this.downloadIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <DownloadIcon /> {/* <SvgIcon component={homemadeDownloadIcon}> */} {/*<DownloadIcon  /> */}
                </IconButton>
              </div>
            </div>
          </div>
          </>
        )
    }
}

export default TableEntry