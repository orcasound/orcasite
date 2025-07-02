import { Box, Button } from "@mui/material";
import { useRouter } from "next/router";

export const SuccessStep = () => {
  const router = useRouter();

  return (
    <Box sx={{ textAlign: "center" }}>
      <Button
        variant="contained"
        color="primary"
        onClick={() => router.push("/")}
        size="large"
      >
        Start listening
      </Button>
    </Box>
  );
};
