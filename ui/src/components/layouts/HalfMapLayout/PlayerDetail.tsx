import { KeyboardArrowDown } from "@mui/icons-material";
import Box from "@mui/material/Box";
import { Dispatch, SetStateAction } from "react";

// placeholder -- after merge, copy BoutPage to this compoonent

export default function PlayerDetail({
  setPlaybarExpanded,
}: {
  setPlaybarExpanded: Dispatch<SetStateAction<boolean>>;
}) {
  return (
    <>
      <Box
        display="flex"
        justifyContent="space-between"
        alignItems="center"
        my={2}
      >
        <KeyboardArrowDown
          sx={{ fontSize: "2rem" }}
          onClick={() => setPlaybarExpanded(false)}
        />
      </Box>
    </>
  );
}
