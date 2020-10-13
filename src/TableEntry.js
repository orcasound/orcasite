import React from "react"

import LikeIcon from "./icons/LikeIcon.js"
import PlayIcon from "./icons/PlayIcon.js"
import ShareIcon from "./icons/ShareIcon"
import DownloadIcon from "./icons/DownloadIcon"

import { IconButton } from "@material-ui/core"

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
                  A user heard a {this.state.type}
              </div>
              <div style={{textAlign:"left", width:"100%"}}>
                <IconButton onClick={this.playIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <PlayIcon />
                </IconButton>
                <IconButton onClick={this.thumbUpIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <LikeIcon /> 
                </IconButton>      
                <IconButton onClick={this.shareIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <ShareIcon /> 
                </IconButton>      
                <IconButton onClick={this.downloadIconButtonClick} disableRipple={true} style={{color: "rgb(0,0,0,0)", width: '35px', height: '35px', backgroundColor: '#f5f5f5', boxShadow: "3px 3px 10px rgba(0, 0, 0, 0.2)"}}>
                  <DownloadIcon />
                </IconButton>
              </div>
            </div>
          </div>
          </>
        )
    }
}

export default TableEntry