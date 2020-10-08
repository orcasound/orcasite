import React from "react"

import {GetAppOutlined as DownloadIcon, ShareOutlined as ShareIcon, PlayCircleOutline as PlayCircleOutlineIcon, ThumbUpOutlined as ThumbUpIcon, Alarm as AlarmIcon, Delete as DeleteIcon, AddShoppingCart as AddShoppingCartIcon} from "@material-ui/icons"
import {makeStyles, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, IconButton, Paper, Box, ThemeProvider} from "@material-ui/core"

import { createMuiTheme, makeStyle} from '@material-ui/core/styles';
import purple from '@material-ui/core/colors/purple';

import OrcaIcon from "./Asset 5.svg"
import ShipIcon from "./Asset 8.svg"
import FishIcon from "./Asset 4.svg"
import SoundIcon from "./Asset 3.svg"


const theme = createMuiTheme({
  palette: {
      primary: { // from default theme
        light: '#4791db',
        main: '#1976d2',
        dark: '#115293'
      },
      secondary: { // from default theme
        light: '#e33371',
        main: '#dc004e',
        dark: '#9a0036',
      },
      text: {
        secondary: '#091033', // light
        primary: '#091033',  // main
        hint: '#091033',  // dark
      },  
      success: {
        main: '#00C56F',
      },
      error: {
        main: '#E9222F',
      },
      informational: {
        main: '#FDB952',
      },
  }, 
  overrides: {
    // colorInherit stores the darkest hue, colorPrimary stores the middle dark hue
    MuiIconButton: {
      colorInherit: {
        color: '#013C74',
      },
      colorPrimary: {
        color: '#346390',
      },   
      colorSecondary: {
        color: '#678AAB',
      }
    },
    //MuiButton: { //purplebutton
    //  colorInherit: {
    //    color: '#7474F9', 
    //  },
    //  colorPrimary: {
    //    color: '#9090FA'
    //  },
    //  colorSecondary: {
    //    color: '#ACACFB',
    //  },
  },

    //link: { (ASK DESIGN TEAM ABOUT WHAT LINKS THEY'RE INTERESTED IN APPLYING THESE COLORS TO)
    //  dark: '#4760FE',
    //  main: '#6C80FE',
    //  light: '#91A0FE',
    // },
    //MuiButton: { //purplebutton
    //  colorInherit: '#7474F9', 
    //  colorPrimary: '#9090FA',
    //  colorSecondary: '#ACACFB',
    //},
    //bluebutton: {
    //  dark: '#2CBDE1',
    //  main: '#56CAE7',
    //  light: '#80D7ED',
    // },
   
});

//import { makeStyles } from '@material-ui/core/styles';
//import Table from '@material-ui/core/Table';
//import TableBody from '@material-ui/core/TableBody';
//import TableCell from '@material-ui/core/TableCell';
//import TableContainer from '@material-ui/core/TableContainer';
//import TableHead from '@material-ui/core/TableHead';
//import TableRow from '@material-ui/core/TableRow';
//import Paper from '@material-ui/core/Paper';


const useStyles = makeStyles({
  table: {
    minWidth: 650,
  },
  /* Source: http://snipplr.com/view/10979/css-cross-browser-word-wrap */
});

function createData(time, eventTypeStr, imageSrc) {
  return { time, eventTypeStr, imageSrc };
}

const rows = [
  createData('9:00 AM August 13 2020', "orca", OrcaIcon),
  createData('1:27 PM August 02 2020', "ship", ShipIcon),
  createData('6:29 AM July 30 2020', "fish", FishIcon),
  createData('6:25 AM July 30 2020', "mystery sound", SoundIcon),
];

export default function SimpleTable() {
  const classes = useStyles();

  return (
    <>

    {/*
    <div display="flex" flexDirection="row">

      <div display="flex" flexDirection="column" flexGrow={1}>
        <div mb={1} bgcolor="grey.300" width="100%">Time</div>
        <div mb={1} >9:00 AM August 13 2020</div>
        <div mb={1} >1:29 PM August 02 2020</div>
        <div mb={1} >6:29 AM July 30 2020</div>
        <div mb={1} >6:25 AM July 30 2020</div>
      </div>

      <div flexGrow={5}>
      
        <div display="flex" flexDirection="row"  mb={1} >
          <div bgcolor="grey.300" width="100%" textAlign="left" flexGrow={1}>Sound</div>
          <div bgcolor="grey.300" width="100%" textAlign="left" flexGrow={1}>Action</div>
        </div>

        <div display="flex" flexDirection="column">

          <div display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={1}>
            <div flexGrow={1} textAlign="left" width="100%">
              A user heard an orca
            </div>
            <div flexGrow={1} textAlign="left" width="100%">
              

              <IconButton divShadow={6} style={{width: '32px', height: '32px', backgroundColor: 'white', divShadow: '2px 3px 2px rgba(0,0,0,.2), -1px 1px 2px rgba(0,0,0,.2)'}} color="inherit" aria-label="huh">
                <PlayCircleOutlineIcon />
              </IconButton>             
              <IconButton color="inherit" aria-label="huh" style={{backgroundColor: 'white'}}>
                <ThumbUpIcon  />  
              </IconButton>      
              <IconButton color="inherit" aria-label="huh">
                <ShareIcon />
              </IconButton>      
              <IconButton color="inherit" aria-label="huh">
                <DownloadIcon  />
              </IconButton>
            </div>
          </div>

          <div display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={1}>
            <div flexGrow={1} textAlign="left" width="100%" >
              <div class="wordwrap">A user heard a ship</div>
            </div>
            <div flexGrow={1} textAlign="left" width="100%">
              Press a button!
            </div>
          </div>

          <div display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={1}>
            <div flexGrow={1} textAlign="left"  width="100%">
              A user heard a fish
            </div>
            <div flexGrow={1} textAlign="left"  width="100%">
              Press a button!
            </div>
          </div>
          
          <div display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={1}>
            <div flexGrow={1} textAlign="left"  width="100%">
              A user heard a mystery sound
            </div>
            <div flexGrow={1} textAlign="left"  width="100%">
              Press a button!
            </div>
          </div>

        </div>

      </div>

    </div>
    */}


    <ThemeProvider theme={theme} >

        <Box display="flex" flexDirection="column">
        
          <Box display="flex" flexDirection="row"  mb={1} >
            <Box bgcolor="grey.300" width="100%" textAlign="left" width="25%">Time</Box>
            <Box bgcolor="grey.300" width="100%" textAlign="left" width="37.5%">Sound</Box>
            <Box bgcolor="grey.300" width="100%" textAlign="left" width="37.5%">Action</Box>
          </Box>

          <Box display="flex" flexDirection="column">

            <Box display="flex" flexGrow={1} width="100%" flexDirection="row" >
              <Box mb={1} className="timeCell" style={{wordwrap: "break-word"}} width="25%">9:00 AM August 13 2020</Box>
              <Box display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={5} borderRadius={3} boxShadow={2} width="75%">              
                <Box flexGrow={1} textAlign="left" width="100%">
                  A user heard an orca
                </Box>
                <Box flexGrow={1} textAlign="left" width="100%">
                  <IconButton boxShadow={6} style={{width: '32px', height: '32px', backgroundColor: 'white', /*boxShadow: '2px 3px 2px rgba(0,0,0,.2), -1px 1px 2px rgba(0,0,0,.2)'*/}} color="inherit" aria-label="huh">
                    <PlayCircleOutlineIcon />
                  </IconButton>             
                  <IconButton color="inherit" aria-label="huh" style={{backgroundColor: 'white'}}>
                    <ThumbUpIcon />  
                  </IconButton>      
                  <IconButton color="inherit" aria-label="huh">
                    <ShareIcon />
                  </IconButton>      
                  <IconButton color="inherit" aria-label="huh">
                    <DownloadIcon  />
                  </IconButton>
                </Box>
              </Box>
            </Box>

            <Box display="flex" flexGrow={1} width="100%" flexDirection="row">
              <Box mb={1} className="timeCell" style={{wordwrap: "break-word"}} width="25%">1:29 PM August 02 2020</Box>
              <Box display="flex"  flexDirection="row" bgcolor="grey.300" ml={1} mb={5} borderRadius={3} boxShadow={2} width="75%">
                <Box textAlign="left" width="100%" >
                  <Box class="wordwrap">A user heard a ship</Box>
                </Box>
                <Box textAlign="left" width="100%">
                  Press a button!
                </Box>
              </Box>
            </Box>

            <Box display="flex" flexgrow={1} width="100%" flexDirection="row">
              <Box mb={1} className="timeCell" style={{wordwrap: "break-word"}} width="25%">6:29 AM July 30 2020</Box>
              <Box  display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={5} borderRadius={3} boxShadow={2} width="75%">
                <Box textAlign="left"  width="100%">
                  A user heard a fish
                </Box>
                <Box textAlign="left"  width="100%">
                  Press a button!
                </Box>
              </Box>
            </Box>
            
            <Box display="flex" width="100%" flexDirection="row">
              <Box mb={1} className="timeCell" style={{wordwrap: "break-word"}} width="25%">6:25 AM July 30 2020</Box>
              <Box display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={5} borderRadius={3} boxShadow={2} width="75%"> 
                <Box textAlign="left"  width="100%">
                  A user heard a mystery sound
                </Box>
                <Box  textAlign="left"  width="100%">
                  Press a button!
                </Box>
              </Box>
            </Box>

          </Box>

        </Box>

    </ThemeProvider>

    <div style={{ width: "100%" }}>
      <Box
        display="flex"
        flexDirection="row"
        p={1}
        m={1}
        bgcolor="background.paper"
      >
        <Box p={1} bgcolor="grey.300" flexGrow={1} ml={1}>
          Item 1
        </Box>
        <Box p={1} bgcolor="grey.300" flexGrow={1} ml={1}>
          Item 2
        </Box>
        <Box p={1} bgcolor="grey.300" flexGrow={1} ml={1}>
          Item 3
        </Box>
      </Box>
    </div>



    <TableContainer component={Paper}>
      <Table className={classes.table} aria-label="simple table">
        <TableHead>
          <TableRow>
            <TableCell>TIME</TableCell>
            <TableCell align="left">SOUND</TableCell>
            <TableCell align="left">ACTION</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {rows.map((row) => (
            <TableRow key={row.name}>
              <TableCell component="th" scope="row">
                {row.time}
              </TableCell>
              <TableCell align="left">
                   A user heard a{row.eventTypeStr == "orca" ? "n" : ""} {row.eventTypeStr}
                  <img src={row.imageSrc} width="50px" height="50px" />
              </TableCell>

              <TableCell align="left">
                <IconButton color="inherit" aria-label="add an alarm">
                    <PlayCircleOutlineIcon />
                </IconButton>
                <IconButton color="primary" aria-label="add an alarm">
                    <ThumbUpIcon />
                </IconButton>
                <IconButton color="secondary" aria-label="add an alarm">
                    <ShareIcon />
                </IconButton>
                <IconButton color="inherit" aria-label="add an alarm">
                    <DownloadIcon />
                </IconButton>

              </TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>

    <TableContainer component={Paper}>
        <Table className={classes.table} aria-label="simple table">
            <TableHead>
            <TableRow>
                <TableCell>TIME</TableCell>
                <TableCell align="right">
                    SOUND ACTION
                </TableCell>
            </TableRow>
            </TableHead>
            <TableBody>
            {rows.map((row) => (
                <TableRow key={row.name}>
                    <TableCell component="th" scope="row">
                        {row.time}
                    </TableCell>
                    <TableCell align="right">
                        <img src={row.imageSrc} width="50px" height="50px" />

                        A user heard a{row.eventTypeStr == "orca" ? "n" : ""} {row.eventTypeStr}
                        <IconButton color="secondary" aria-label="add an alarm">
                            <PlayCircleOutlineIcon />
                        </IconButton>
                        <IconButton color="secondary" aria-label="add an alarm">
                            <ThumbUpIcon />
                        </IconButton>
                        <IconButton color="secondary" aria-label="add an alarm">
                            <ShareIcon />
                        </IconButton>
                        <IconButton color="secondary" aria-label="add an alarm">
                            <DownloadIcon />
                        </IconButton>

                    </TableCell>
                </TableRow>
            ))}
            </TableBody>
        </Table>
    </TableContainer>

  </>
  );
}