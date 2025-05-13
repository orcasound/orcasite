import { Earbuds, Mic } from "@mui/icons-material";
import { BottomNavigation, BottomNavigationAction } from "@mui/material";

type Props = {
  menuTab: number;
  setMenuTab: React.Dispatch<React.SetStateAction<number>>;
};

export function MobileBottomNav({ menuTab, setMenuTab }: Props) {
  return (
    <BottomNavigation
      showLabels
      value={menuTab}
      onChange={(_event, newMenuTab) => {
        setMenuTab(newMenuTab);
      }}
      sx={{ height: "69px" }}
    >
      <BottomNavigationAction label="Recordings" icon={<Earbuds />} />
      <BottomNavigationAction label="Listen Live" icon={<Mic />} />
    </BottomNavigation>
  );
}
