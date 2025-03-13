import BarChartIcon from "@mui/icons-material/BarChart";
import DataObjectIcon from "@mui/icons-material/DataObject";
import EarbudsIcon from "@mui/icons-material/Earbuds";
import MicIcon from "@mui/icons-material/Mic";
import PlaceIcon from "@mui/icons-material/Place";
import PlayCircleOutlineIcon from "@mui/icons-material/PlayCircleOutline";

const navigationModerator = [
  {
    kind: "subheader",
    title: "",
    children: [
      {
        title: "Map",
        path: "/map",
        icon: <PlaceIcon fontSize="inherit" />,
      },
      {
        title: "Livestreams",
        path: "/moderator/bouts/",
        icon: <MicIcon fontSize="inherit" />,
      },
      {
        title: "Recordings",
        path: "/moderator/candidates",
        icon: <PlayCircleOutlineIcon fontSize="inherit" />,
      },
      {
        title: "Tutorial",
        path: "/moderator/learn/",
        icon: <EarbudsIcon fontSize="inherit" />,
      },
      {
        title: "Archive",
        path: "/moderator/reports",
        icon: <BarChartIcon fontSize="inherit" />,
      },
      {
        title: "JSON",
        path: "/moderator/json",
        icon: <DataObjectIcon fontSize="inherit" />,
      },
    ],
  },
  //  {    kind: "divider",  },
];

export default navigationModerator;
