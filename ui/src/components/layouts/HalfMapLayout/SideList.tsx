import { Box } from "@mui/material";

type SideListProps = {
  children: React.ReactNode;
};

export const SideList = ({ children }: SideListProps) => {
  return (
    <Box
      className="side-list"
      sx={{
        borderRightColor: "divider",
        borderRightStyle: "solid",
        borderRightWidth: 1,
        width: "25%",
        minWidth: "367px",
        overflow: "auto",
      }}
    >
      {children}
    </Box>
  );
};
