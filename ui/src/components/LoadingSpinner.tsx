import { Box, BoxProps, CircularProgress } from "@mui/material";

export default function LoadingSpinner(params: BoxProps) {
  return (
    <Box display="flex" justifyContent="center" alignItems="center" {...params}>
      <CircularProgress />
    </Box>
  );
}
