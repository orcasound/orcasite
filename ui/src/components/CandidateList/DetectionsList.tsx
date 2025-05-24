import { AccountCircle, Edit } from "@mui/icons-material";
import {
  Box,
  List,
  ListItemAvatar,
  ListItemButton,
  ListItemText,
} from "@mui/material";

import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
import { CombinedData } from "@/types/DataTypes";

export const DetectionsList = (detections: CombinedData[], feed: Feed) => {
  const { filteredData } = useData();
  const detectionsThisFeed = filteredData.filter((d) => d.feedId === feed?.id);
  const userName = "UserProfile123";
  const aiName = "Orcahello AI";

  return (
    <List>
      {detectionsThisFeed?.map((el, index) => (
        <ListItemButton key={index}>
          <ListItemAvatar>
            <AccountCircle style={{ fontSize: 40, opacity: 0.9 }} />
          </ListItemAvatar>
          <ListItemText
            className="list-item-text"
            primary={
              (el.newCategory !== "WHALE (AI)" ? userName : aiName) +
              " • " +
              new Date(el.timestampString).toLocaleString()
            }
            secondary={`${el.hydrophone} • ${el.newCategory} ${el.comments ? "• " + el.comments : ""}`}
          />
          <ListItemAvatar sx={{ display: "flex", opacity: "0.9" }}>
            <Edit />
            <Box sx={{ padding: "0 8px" }} />
            <Box sx={{ padding: "0 8px" }} />
          </ListItemAvatar>
        </ListItemButton>
      ))}
    </List>
  );
};
