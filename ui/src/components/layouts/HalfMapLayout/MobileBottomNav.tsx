import { Earbuds, Map, Mic, Public } from "@mui/icons-material";
import { BottomNavigation, BottomNavigationAction } from "@mui/material";
import React, { Dispatch, SetStateAction } from "react";

type Props = {
  menuTab: number;
  setMenuTab: Dispatch<SetStateAction<number>>;
};

export function MobileBottomNav({ menuTab, setMenuTab }: Props) {
  return (
    <BottomNavigation
      className="bottom-navigation"
      showLabels
      value={menuTab}
      onChange={(_event, newMenuTab) => {
        setMenuTab(newMenuTab);
      }}
      sx={{ minHeight: "69px" }}
    >
      <BottomNavigationAction label="Map" icon={<Map />} />
      <BottomNavigationAction label="Listen Live" icon={<Mic />} />
      <BottomNavigationAction label="Explore" icon={<Earbuds />} />
      <BottomNavigationAction label="Take Action" icon={<Public />} />
    </BottomNavigation>
  );
}
