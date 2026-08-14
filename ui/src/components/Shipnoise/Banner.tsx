import { Box, Button, Link, Stack, Typography } from "@mui/material";
import { useState } from "react";

const Banner = () => {
  const [showReportForm, setShowReportForm] = useState(false);

  const openReportForm = () => setShowReportForm(true);
  const closeReportForm = () => setShowReportForm(false);

  return (
    <>
      <Box
        component="header"
        sx={{
          zIndex: 50,
          width: "100%",
          bgcolor: "black",
          boxShadow: { xs: "0 4px 6px rgba(0,0,0,0.1)", sm: "none" },
        }}
      >
        <Box
          sx={{
            width: "100%",
            px: { xs: 2, sm: 3, lg: "35px" },
            py: { xs: 1.5, sm: 2, lg: 2.5 },
          }}
        >
          <Stack
            direction="row"
            alignItems="center"
            justifyContent="space-between"
            sx={{ width: "100%" }}
          >
            <Stack direction="row" alignItems="center" spacing={1.5}>
              {/* eslint-disable-next-line @next/next/no-img-element */}
              <img
                src="/shipnoise/Logo.png"
                alt="Shipnoise Logo"
                width={38}
                height={40}
              />
              <Typography
                component="h1"
                sx={{
                  color: "white",
                  fontSize: { xs: "22px", sm: "24px" },
                  fontWeight: 700,
                  fontFamily: "Mukta, sans-serif",
                }}
              >
                Shipnoise
              </Typography>
            </Stack>

            <Stack
              direction={{ xs: "column", sm: "row" }}
              spacing={{ xs: 1.5, sm: 2 }}
              alignItems={{ xs: "flex-start", sm: "center" }}
            >
              <Link
                href="https://mailchi.mp/7ce0cea69cd0/help-improve-shipnoise"
                target="_blank"
                rel="noreferrer"
                underline="none"
                sx={{ width: { xs: "auto", sm: 224 } }}
              >
                <Button
                  variant="contained"
                  fullWidth
                  sx={{
                    height: 38,
                    borderRadius: "100px",
                    bgcolor: "white",
                    color: "black",
                    textTransform: "none",
                    fontSize: "15px",
                    fontWeight: 500,
                    fontFamily: "Montserrat, sans-serif",
                    "&:hover": {
                      bgcolor: "#f1f5f9",
                    },
                  }}
                >
                  Help improve Shipnoise!
                </Button>
              </Link>

              <Button
                type="button"
                onClick={openReportForm}
                variant="outlined"
                sx={{
                  height: 38,
                  width: { xs: "auto", sm: 232 },
                  borderRadius: "100px",
                  borderColor: "white",
                  color: "white",
                  textTransform: "none",
                  fontSize: "15px",
                  fontWeight: 500,
                  fontFamily: "Montserrat, sans-serif",
                  "&:hover": {
                    borderColor: "white",
                    bgcolor: "rgba(255,255,255,0.1)",
                  },
                }}
              >
                Report Technical Problem
              </Button>
            </Stack>
          </Stack>
        </Box>
      </Box>

      {showReportForm && (
        <Box
          sx={{
            position: "fixed",
            inset: 0,
            zIndex: 50,
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            bgcolor: "rgba(0,0,0,0.7)",
            px: 2,
            py: 2.5,
          }}
        >
          <Box
            sx={{
              position: "relative",
              width: "100%",
              maxWidth: 760,
              height: "100%",
              maxHeight: "90vh",
              borderRadius: 4,
              bgcolor: "white",
              boxShadow: "0 25px 50px -12px rgba(0,0,0,0.25)",
              overflow: "hidden",
            }}
          >
            <Button
              type="button"
              onClick={closeReportForm}
              variant="contained"
              sx={{
                position: "absolute",
                right: 16,
                top: 16,
                borderRadius: "999px",
                bgcolor: "rgba(0,0,0,0.7)",
                color: "white",
                textTransform: "none",
                fontSize: "14px",
                fontWeight: 500,
                fontFamily: "Montserrat, sans-serif",
                px: 2,
                py: 0.5,
                minHeight: "auto",
                "&:hover": {
                  bgcolor: "rgba(0,0,0,0.75)",
                },
              }}
            >
              Close
            </Button>

            <Box
              component="iframe"
              title="Report Technical Problem"
              src="https://tally.so/embed/3E4Z6X?hideTitle=1&transparentBackground=1&formEventsForwarding=1"
              sx={{ height: "100%", width: "100%", border: 0 }}
              allow="fullscreen"
            />
          </Box>
        </Box>
      )}
    </>
  );
};

export default Banner;
