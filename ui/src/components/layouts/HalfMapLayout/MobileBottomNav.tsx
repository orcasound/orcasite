import { Earbuds, Mic } from "@mui/icons-material";
import { BottomNavigation, BottomNavigationAction } from "@mui/material";
import React, { Dispatch, SetStateAction } from "react";

type Props = {
  menuTab: number;
  setMenuTab: Dispatch<SetStateAction<number>>;
  setTabValue: Dispatch<SetStateAction<number>>;
};

export function MobileBottomNav({ menuTab, setMenuTab, setTabValue }: Props) {
  return (
    <BottomNavigation
      showLabels
      value={menuTab}
      onChange={(_event, newMenuTab) => {
        setMenuTab(newMenuTab);
        setTabValue(0);
      }}
      sx={{ height: "69px" }}
    >
      <BottomNavigationAction label="Recordings" icon={<Earbuds />} />
      <BottomNavigationAction label="Listen Live" icon={<Mic />} />
    </BottomNavigation>
  );
}
