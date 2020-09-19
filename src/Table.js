import React from "react"

import {GetApp as DownloadIcon, Share as ShareIcon, PlayCircleOutline as PlayCircleOutlineIcon, ThumbUp as ThumbUpIcon, Alarm as AlarmIcon, Delete as DeleteIcon, AddShoppingCart as AddShoppingCartIcon} from "@material-ui/icons"
import {makeStyles, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, IconButton, Paper} from "@material-ui/core"

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
    <TableContainer component={Paper}>
      <Table className={classes.table} aria-label="simple table">
        <TableHead>
          <TableRow>
            <TableCell>TIME</TableCell>
            <TableCell align="right">SOUND</TableCell>
            <TableCell align="right">ACTION</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {rows.map((row) => (
            <TableRow key={row.name}>
              <TableCell component="th" scope="row">
                {row.time}
              </TableCell>
              <TableCell align="right">
                  <img src={row.imageSrc} />
              </TableCell>

              <TableCell align="right">
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
  );
}