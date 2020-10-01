import React from "react"

import {GetApp as DownloadIcon, Share as ShareIcon, PlayCircleOutline as PlayCircleOutlineIcon, ThumbUp as ThumbUpIcon, Alarm as AlarmIcon, Delete as DeleteIcon, AddShoppingCart as AddShoppingCartIcon} from "@material-ui/icons"
import {makeStyles, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, IconButton, Paper, Box} from "@material-ui/core"

//import { makeStyles } from '@material-ui/core/styles';
//import Table from '@material-ui/core/Table';
//import TableBody from '@material-ui/core/TableBody';
//import TableCell from '@material-ui/core/TableCell';
//import TableContainer from '@material-ui/core/TableContainer';
//import TableHead from '@material-ui/core/TableHead';
//import TableRow from '@material-ui/core/TableRow';
//import Paper from '@material-ui/core/Paper';

import OrcaIcon from "./Asset 5.svg"
import ShipIcon from "./Asset 8.svg"
import FishIcon from "./Asset 4.svg"
import SoundIcon from "./Asset 3.svg"

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


      <Box display="flex" flexDirection="row">

        <Box display="flex" flexDirection="column" flexGrow={1}>
          <Box mb={1} bgcolor="grey.300" width="100%">Time</Box>
          <Box mb={1} >Detection 1</Box>
          <Box mb={1} >Detection 2</Box>
          <Box mb={1} >Detection 3</Box>
        </Box>

        <Box flexGrow={5}>
        
          <Box display="flex" flexDirection="row"  mb={1} >
            <Box bgcolor="grey.300" width="100%" textAlign="left" flexGrow={1}>Sound</Box>
            <Box bgcolor="grey.300" width="100%" textAlign="left" flexGrow={1}>Action</Box>
          </Box>

          <Box display="flex" flexDirection="column">

            <Box display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={1}>
              <Box flexGrow={1} textAlign="left" width="100%">
                A user heard an orca
              </Box>
              <Box flexGrow={1} textAlign="left" width="100%">
                Press a button!
              </Box>
            </Box>

            <Box display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={1}>
              <Box flexGrow={1} textAlign="left" width="100%" >
                <Box class="wordwrap">A user heard a pasdfsdksjldfjlskldfjfsdfsdfsdfsdf</Box>
              </Box>
              <Box flexGrow={1} textAlign="left" width="100%">
                Press a button!
              </Box>
            </Box>

            <Box display="flex" flexDirection="row" bgcolor="grey.300" ml={1} mb={1}>
              <Box flexGrow={1} textAlign="left"  width="100%">
                A user heard a mystery sound
              </Box>
              <Box flexGrow={1} textAlign="left"  width="100%">
                Press a button!
              </Box>
            </Box>

          </Box>

        </Box>

      </Box>


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