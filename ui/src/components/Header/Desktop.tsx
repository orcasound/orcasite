import { Notifications } from "@mui/icons-material";
import { Box, Button } from "@mui/material";

import Link from "@/components/Link";
import { displayDesktopOnly } from "@/styles/responsive";
import { analytics } from "@/utils/analytics";

import UserMenu from "./AccountMenu";
import Brand from "./Brand";

export default function Desktop() {
  const pages = [
    {
      label: "About us",
      url: "https://www.orcasound.net/",
      onClick: () => analytics.nav.aboutTabClicked(),
    },
    {
      label: "Send feedback",
      url: "https://forms.gle/wKpAnxzUh9a5LMfd7",
      onClick: () => analytics.nav.feedbackTabClicked(),
    },
  ];
  return (
    <Box sx={{ ...displayDesktopOnly, width: "100%" }}>
      <Box
        sx={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          width: 1,
        }}
      >
        <Brand />
        <Box sx={{ display: "flex" }}>
          {pages.map((page) => (
            <Button
              onClick={() => page.onClick && page.onClick()}
              href={page.url}
              target="_blank"
              key={page.label}
              variant="text"
              sx={{
                my: 2,
                mx: 1,
                color: "base.contrastText",
                textWrap: "nowrap",
              }}
            >
              {page.label}
            </Button>
          ))}
          <Link
            href="https://docs.google.com/forms/d/1oYSTa3QeAAG-G_eTxjabrXd264zVARId9tp2iBRWpFs/edit"
            title="Get notified when there's whale activity."
            target="_blank"
            sx={{
              my: 2,
              mx: 1,
              textWrap: "nowrap",
            }}
            onClick={() => analytics.nav.notificationsClicked()}
          >
            <Button
              variant="contained"
              color="primary"
              startIcon={<Notifications sx={{ color: "gold" }} />}
              sx={{
                borderRadius: 8,
              }}
            >
              Get notified
            </Button>
          </Link>
          <UserMenu sx={{ my: 2, mx: 1, textWrap: "nowrap" }} />
        </Box>
      </Box>
    </Box>
  );
}
