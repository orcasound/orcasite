import EarbudsIcon from "@mui/icons-material/Earbuds";
import PlaceIcon from "@mui/icons-material/Place";
import PlayCircleOutlineIcon from "@mui/icons-material/PlayCircleOutline";

const navigationHalfMap = [
  {
    kind: "subheader",
    title: "Listen Live",
    children: [
      {
        title: "Locations",
        path: "/concept/hydrophones",
        icon: <PlaceIcon fontSize="inherit" />,
      },
      {
        title: "Learn",
        path: "/concept/learn/",
        icon: <EarbudsIcon fontSize="inherit" />,
      },
    ],
  },
  { kind: "divider" },
  {
    kind: "subheader",
    title: "Moderate",
    children: [
      {
        title: "Bout Monitor",
        path: "/concept/bouts",
        icon: <PlaceIcon fontSize="inherit" />,
      },
      {
        title: "Candidates",
        path: "/concept/candidates/",
        icon: <EarbudsIcon fontSize="inherit" />,
      },
      {
        title: "Channels",
        path: "#",
        icon: <EarbudsIcon fontSize="inherit" />,
      },
    ],
  },
  { kind: "divider" },
  {
    kind: "subheader",
    title: "Analyze",
    children: [
      {
        title: "Acartia",
        path: "#",
        icon: <PlaceIcon fontSize="inherit" />,
      },
      {
        title: "Salmon",
        path: "#",
        icon: <EarbudsIcon fontSize="inherit" />,
      },
      {
        title: "Shipnoise",
        path: "#",
        icon: <PlayCircleOutlineIcon fontSize="inherit" />,
      },
    ],
  },
  { kind: "divider" },
  {
    kind: "subheader",
    title: "System Status",
    children: [
      {
        title: "Node Monitor",
        path: "#",
        icon: <PlaceIcon fontSize="inherit" />,
      },
      {
        title: "User Analytics",
        path: "#",
        icon: <EarbudsIcon fontSize="inherit" />,
      },
      {
        title: "AI Model",
        path: "#",
        icon: <PlayCircleOutlineIcon fontSize="inherit" />,
      },
    ],
  },
];

export default navigationHalfMap;
