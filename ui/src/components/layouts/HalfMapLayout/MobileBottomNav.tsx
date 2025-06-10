import { Earbuds, Map, Mic, Public } from "@mui/icons-material";
import { BottomNavigation, BottomNavigationAction } from "@mui/material";
import { useRouter } from "next/router";
import React from "react";

import { useLayout } from "@/context/LayoutContext";

export function MobileBottomNav() {
  const router = useRouter();
  const { mobileTab, setMobileTab } = useLayout();

  return (
    <BottomNavigation
      className="bottom-navigation"
      showLabels
      value={mobileTab}
      onChange={(_event, newMenuTab) => {
        setMobileTab(newMenuTab);
      }}
      sx={{
        minHeight: "69px",
        backgroundColor: "background.paper",
        "& .MuiBottomNavigationAction-root": {
          color: "text.secondary",
          "& .MuiSvgIcon-root": {
            color: "text.secondary",
          },
        },
        "& .Mui-selected": {
          color: "text.primary",
          "& .MuiSvgIcon-root": {
            color: "text.primary",
          },
        },
      }}
    >
      <BottomNavigationAction
        label="Map"
        icon={<Map />}
        onClick={() => router.push("/beta")}
      />
      <BottomNavigationAction
        label="Hydrophones"
        icon={<Mic />}
        onClick={() => router.push("/beta/hydrophones")}
      />
      <BottomNavigationAction label="Explore" icon={<Earbuds />} />
      <BottomNavigationAction label="Take Action" icon={<Public />} />
    </BottomNavigation>
  );
}
