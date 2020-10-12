import React, { useState } from "react"
import { Link as RouterLink } from "react-router-dom"

import FeedList from "./FeedList"

import SimpleMap from "./SimpleMap.js"

import About from "./About.js"

import ListenPageRoot from "./ListenPageRoot.js"

import { Query } from "react-apollo"

import Loader from "components/Loader"

import {LIST_DETECTIONS} from "../queries/detections"


import {
  Paper,
  AppBar,
  Toolbar,
  Tabs,
  Tab,
  Button,
  Typography,
  Box,
  Link,
  IconButton,
  makeStyles,
  Card,
  CardActionArea,
  CardMedia,
  CardContent,
  Grid,
} from "@material-ui/core"
import NotificationIcon from "@material-ui/icons/Notifications"

import OrcasoundLogo from "../../static/orcasound-logo.png"
import OrcaImage from  "../../static/spyhops_ship_updated.jpg"
import OtherLogoSvgFile from "./Asset3.svg"

//import OtherLogo from "Asset_3.svg"

import OtherLogo from "./OtherIcon"


import FormDialog from "./FormDialog.js"

import { sizing } from '@material-ui/system'


const useStyles = makeStyles(theme => ({
  button: {
    minWidth: "180px",
    borderRadius: 0,
    "&:hover": {
      textDecoration: "none",
      color: "#000000"
    },
    "&:active": {
      color: "#15766b"
    },
    "&:visited": {
      color: "#000000"
    }
  },
  link: {
    "&:hover": {
      textDecoration: "none",
      color: "#ffffff"
    },
    "&:active": {
      color: "#15766b"
    },
    "&:visited": {
      color: "#ffffff"
    }
  },
  rightToolBar: {
      marginLeft: "auto"
  },
  logoImage: {
      height: "200px;",
      width: "200px;"
  },
  navBarIconButton: {
    color: "white",
    gutterTop: "20px",
    gutterBottom: "20px",
    disableGutters: false,
  },
  frontPageOrcaImageBox: {
    height: '25%'
  },
  navBarLink: {
    color: "white",
    textTransform: "none",
  }
}))

const NewHomeSiteMenu = () => {
  const classes = useStyles()
  const [value, setValue] = useState("about")
 
  /*
  const location = {
    address: '1600 Amphitheatre Parkway, Mountain View, california.',
    lat: 37.42216,
    lng: -122.08427,
  } 
  */

  const notificationDoc = `https://docs.google.com/forms/d/1oYSTa3QeAAG-G_eTxjabrXd264zVARId9tp2iBRWpFs/edit`

  const handleChange = (e, newValue) => {
    setValue(newValue)
  }

  return (
    <>
    <Paper elevation={0} square>
      <AppBar position="static" color="inherit" className={classes.appBar}>
        <Toolbar variant="dense">
          <Box flexGrow={1}>

            <Box display="flex" flexDirection="column" justifyContent="center">
              <Box display="flex" flexDirection="row">

                <img
                  alt="Orcasound Logo"
                  src={OrcasoundLogo}
                  style={{width: "110px", height: "110px"}}
                />

                <Box display="flex" flexDirection="column" justifyContent="center">
                  <a
                    href="/"
                    underline="none"
                  >
                    <h1 style={{fontFamily: "Mukta"}}>Orcasound</h1>
                  </a>
                </Box>
              </Box>
            </Box>
          </Box>

          <Box display="flex" flexDirection="column" justifyContent="center">
            <div style={{display:"flex", flexDirection:"row"}}>
              <div style={{fontFamily: "Montserrat", fontWeight: "400", fontStyle: "normal", fontSize: "20px", lineHeight: "28px"}}>             
                <a href={"/listen"} className={classes.navBarLink}>
                  Listen
                </a>
              </div>
              <div color="inherit" style={{fontFamily: "Montserrat", fontWeight: "400", fontStyle: "normal", fontSize: "20px", lineHeight: "28px"}}>
                <a href={"/learn"} className={classes.navBarLink}>
                  Learn
                  </a>
              </div>
              <div color="inherit" style={{fontFamily: "Montserrat", fontWeight: "400", fontStyle: "normal", fontSize: "20px", lineHeight: "28px"}}>
                <a href={"/projects"} className={classes.navBarLink}>
                  Projects
                </a>
              </div>
              <div className={classes.navBarIcondiv} color="inherit" style={{fontFamily: "Montserrat", fontWeight: "400", fontStyle: "normal", fontSize: "20px", lineHeight: "28px"}}>
                <a className={classes.navBarLink}>
                    Blogs
                </a>
              </div>
              <div className={classes.navBarIcondiv} color="inherit" style={{fontFamily: "Montserrat", fontWeight: "400", fontStyle: "normal", fontSize: "20px", lineHeight: "28px"}}>
                <a href="/about" className={classes.navBarLink}>
                  About
                </a>
              </div>
              <div className={classes.navBarIcondiv} color="inherit" style={{fontFamily: "Montserrat", fontWeight: "400", fontStyle: "normal", fontSize: "20px", lineHeight: "28px"}}>
                <a href={"/support"} className={classes.navBarLink}>
                  Support
                </a>
              </div>
              <FormDialog />
            </div>
        </Box>

        </Toolbar>
      </AppBar>
    </Paper>  
    {/*
    <Paper elevation={0} square>
      <AppBar position="static" color="inherit" className={classes.appBar}>
        <Toolbar variant="dense">
          <Box flexGrow={1}>

          <Box display="flex" flexDirection="column" justifyContent="center">
            <Box display="flex" flexDirection="row">

              <CardMedia
                component="img"
                alt="Orcasound Logo"
                className={classes.logoImage}
                image={OrcasoundLogo}
              />

              <Box display="flex" flexDirection="column" justifyContent="center">
                <Typography component="h1" variant="h1">
                  <Link
                    component={RouterLink}
                    to="/"
                    color="inherit"
                    underline="none"
                    variant="inherit"
                    className={classes.link}
                  >
                    <Box ml={1} pt={0} >
                      <h1>Orcasound</h1>
                    </Box>
                  </Link>
                </Typography>
              </Box>
            </Box>
          </Box>
          </Box>

          <Box display="flex" flexDirection="column" justifyContent="center">
            <div className={classes.rightToolBar}>
              <Button color="inherit" >             
                <Link href={"/listen"} className={classes.navBarLink}>
                  Listen
                </Link>
              </Button>
              <Button className={classes.navBarIconButton} color="inherit">
                <Link href={"/learn"} className={classes.navBarLink}>
                  Learn
                  </Link>
              </Button>
              <Button className={classes.navBarIconButton} color="inherit">
                <Link href={"/projects"} className={classes.navBarLink}>
                  Projects
                </Link>
              </Button>
              <Button className={classes.navBarIconButton} color="inherit">
                <Link className={classes.navBarLink}>
                    Blogs
                </Link>
              </Button>
              <Button className={classes.navBarIconButton} color="inherit" >
                <Link href="/about" className={classes.navBarLink}>
                  About
                </Link>
              </Button>
              <Button className={classes.navBarIconButton} color="inherit">
                <Link href={"/support"} className={classes.navBarLink}>
                  Support
                </Link>
              </Button>
              <FormDialog className={classes.navBarIconButton}/>
            </div>
        </Box>

        </Toolbar>
      </AppBar>
    </Paper>
     */}
    <Grid container spacing={1}>
      <Grid item xs={6}>
          <About />
           <Box className={classes.frontPageOrcaImageBox}>
          {/*<img src={OtherLogoSvgFile} />*/}
          <OtherLogoSvgFile width="50px" height="50px"/>
          </Box>

      </Grid>
      <Grid item xs={6}>
        <SimpleMap />
      </Grid>
    </Grid>

      <Query query={LIST_DETECTIONS}>
        {({data, loading, error}) => {
            if (loading) {
              return <Loader />
            }

            if (error || !data) {
              return <div>Error detections data not available</div>
            }

            debugger;
   
            return (
              <>
                <p>
                  Detections:
                </p>
                <>
                  {/*data*/}
                </>
              </>
            )
    
          }}
      </Query>

    </>
  )
}

export default NewHomeSiteMenu
