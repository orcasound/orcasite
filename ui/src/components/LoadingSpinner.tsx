import { Box, BoxProps, CircularProgress } from "@mui/material";

export default function LoadingSpinner(params: BoxProps) {
  return (
    <Box
      {...params}
      sx={[
        {
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
        },
        ...(Array.isArray(params.sx) ? params.sx : [params.sx]),
      ]}
    >
      <CircularProgress />
    </Box>
  );
}
