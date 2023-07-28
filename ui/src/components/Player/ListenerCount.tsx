import { Person } from "@mui/icons-material";
import { Box } from "@mui/material";

export default function ListenerCount({ count }: { count: number }) {
  return (
    <Box
      sx={{
        display: "flex",
        alignItems: "center",
      }}
    >
      <Person sx={{ mr: 1 }} />
      {count}
    </Box>
  );
}
