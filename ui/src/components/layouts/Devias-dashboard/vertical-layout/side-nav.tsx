// import File04Icon from '@untitled-ui/icons-react/build/esm/File04';
import { Drawer, Link, Stack, Typography } from "@mui/material";
// import { Logo } from '../../../components/logo';
import Image from "next/image";
import type { FC } from "react";

import wordmark from "@/public/wordmark/wordmark-white.svg";
import { analytics } from "@/utils/analytics";

// import { Scrollbar } from '../../../components/scrollbar';
// import { paths } from '../../../paths';
// import type { NavColor } from '../../../types/settings';
import type { Section } from "../config";

const SIDE_NAV_WIDTH: string = "280px";

function Brand({ onClick }: { onClick?: () => void }) {
  return (
    <Typography variant="h6" noWrap overflow="visible">
      <Link
        href="/"
        color="inherit"
        underline="none"
        sx={{
          height: "100%",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
        }}
        onClick={() => {
          if (onClick) onClick();
          analytics.nav.logoClicked();
        }}
      >
        <Image
          src={wordmark.src}
          alt="Orcasound"
          width={140}
          height={60}
          priority={true}
        />
      </Link>
    </Typography>
  );
}

// const useCssVars = (color: NavColor): Record<string, string> => {
//   const theme = useTheme();

//   return useMemo(
//     (): Record<string, string> => {
//       switch (color) {
//         case 'blend-in':
//           if (theme.palette.mode === 'dark') {
//             return {
//               '--nav-bg': theme.palette.background.default,
//               '--nav-color': theme.palette.neutral[100],
//               '--nav-border-color': theme.palette.neutral[700],
//               '--nav-logo-border': theme.palette.neutral[700],
//               '--nav-section-title-color': theme.palette.neutral[400],
//               '--nav-item-color': theme.palette.neutral[400],
//               '--nav-item-hover-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-color': theme.palette.text.primary,
//               '--nav-item-disabled-color': theme.palette.neutral[600],
//               '--nav-item-icon-color': theme.palette.neutral[500],
//               '--nav-item-icon-active-color': theme.palette.primary.main,
//               '--nav-item-icon-disabled-color': theme.palette.neutral[700],
//               '--nav-item-chevron-color': theme.palette.neutral[700],
//               '--nav-scrollbar-color': theme.palette.neutral[400]
//             };
//           } else {
//             return {
//               '--nav-bg': theme.palette.background.default,
//               '--nav-color': theme.palette.text.primary,
//               '--nav-border-color': theme.palette.neutral[100],
//               '--nav-logo-border': theme.palette.neutral[100],
//               '--nav-section-title-color': theme.palette.neutral[400],
//               '--nav-item-color': theme.palette.text.secondary,
//               '--nav-item-hover-bg': theme.palette.action.hover,
//               '--nav-item-active-bg': theme.palette.action.selected,
//               '--nav-item-active-color': theme.palette.text.primary,
//               '--nav-item-disabled-color': theme.palette.neutral[400],
//               '--nav-item-icon-color': theme.palette.neutral[400],
//               '--nav-item-icon-active-color': theme.palette.primary.main,
//               '--nav-item-icon-disabled-color': theme.palette.neutral[400],
//               '--nav-item-chevron-color': theme.palette.neutral[400],
//               '--nav-scrollbar-color': theme.palette.neutral[900]
//             };
//           }

//         case 'discreet':
//           if (theme.palette.mode === 'dark') {
//             return {
//               '--nav-bg': theme.palette.neutral[900],
//               '--nav-color': theme.palette.neutral[100],
//               '--nav-border-color': theme.palette.neutral[700],
//               '--nav-logo-border': theme.palette.neutral[700],
//               '--nav-section-title-color': theme.palette.neutral[400],
//               '--nav-item-color': theme.palette.neutral[400],
//               '--nav-item-hover-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-color': theme.palette.text.primary,
//               '--nav-item-disabled-color': theme.palette.neutral[600],
//               '--nav-item-icon-color': theme.palette.neutral[500],
//               '--nav-item-icon-active-color': theme.palette.primary.main,
//               '--nav-item-icon-disabled-color': theme.palette.neutral[700],
//               '--nav-item-chevron-color': theme.palette.neutral[700],
//               '--nav-scrollbar-color': theme.palette.neutral[400]
//             };
//           } else {
//             return {
//               '--nav-bg': theme.palette.neutral[50],
//               '--nav-color': theme.palette.text.primary,
//               '--nav-border-color': theme.palette.divider,
//               '--nav-logo-border': theme.palette.neutral[200],
//               '--nav-section-title-color': theme.palette.neutral[500],
//               '--nav-item-color': theme.palette.neutral[500],
//               '--nav-item-hover-bg': theme.palette.action.hover,
//               '--nav-item-active-bg': theme.palette.action.selected,
//               '--nav-item-active-color': theme.palette.text.primary,
//               '--nav-item-disabled-color': theme.palette.neutral[400],
//               '--nav-item-icon-color': theme.palette.neutral[400],
//               '--nav-item-icon-active-color': theme.palette.primary.main,
//               '--nav-item-icon-disabled-color': theme.palette.neutral[400],
//               '--nav-item-chevron-color': theme.palette.neutral[400],
//               '--nav-scrollbar-color': theme.palette.neutral[900]
//             };
//           }

//         case 'evident':
//           if (theme.palette.mode === 'dark') {
//             return {
//               '--nav-bg': theme.palette.neutral[800],
//               '--nav-color': theme.palette.common.white,
//               '--nav-border-color': 'transparent',
//               '--nav-logo-border': theme.palette.neutral[700],
//               '--nav-section-title-color': theme.palette.neutral[400],
//               '--nav-item-color': theme.palette.neutral[400],
//               '--nav-item-hover-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-color': theme.palette.common.white,
//               '--nav-item-disabled-color': theme.palette.neutral[500],
//               '--nav-item-icon-color': theme.palette.neutral[400],
//               '--nav-item-icon-active-color': theme.palette.primary.main,
//               '--nav-item-icon-disabled-color': theme.palette.neutral[500],
//               '--nav-item-chevron-color': theme.palette.neutral[600],
//               '--nav-scrollbar-color': theme.palette.neutral[400]
//             };
//           } else {
//             return {
//               '--nav-bg': theme.palette.neutral[800],
//               '--nav-color': theme.palette.common.white,
//               '--nav-border-color': 'transparent',
//               '--nav-logo-border': theme.palette.neutral[700],
//               '--nav-section-title-color': theme.palette.neutral[400],
//               '--nav-item-color': theme.palette.neutral[400],
//               '--nav-item-hover-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-bg': 'rgba(255, 255, 255, 0.04)',
//               '--nav-item-active-color': theme.palette.common.white,
//               '--nav-item-disabled-color': theme.palette.neutral[500],
//               '--nav-item-icon-color': theme.palette.neutral[400],
//               '--nav-item-icon-active-color': theme.palette.primary.main,
//               '--nav-item-icon-disabled-color': theme.palette.neutral[500],
//               '--nav-item-chevron-color': theme.palette.neutral[600],
//               '--nav-scrollbar-color': theme.palette.neutral[400]
//             };
//           }

//         default:
//           return {};
//       }
//     },
//     [theme, color]
//   );
// };

interface SideNavProps {
  // color?: NavColor;
  sections?: Section[];
}

export const SideNav: FC<SideNavProps> = (_props) => {
  // const { color = 'evident', sections = [] } = props;
  // const pathname = usePathname();
  // const cssVars = useCssVars(color);

  return (
    <Drawer
      anchor="left"
      open
      sx={{ width: SIDE_NAV_WIDTH }}
      PaperProps={{
        sx: {
          // ...cssVars,
          // backgroundColor: 'var(--nav-bg)',
          borderRightColor: "var(--nav-border-color)",
          borderRightStyle: "solid",
          borderRightWidth: 1,
          // color: 'var(--nav-color)',
          width: SIDE_NAV_WIDTH,
          backgroundColor: "base.main",
          color: "base.contrastText",
        },
      }}
      variant="permanent"
    >
      {/* <Scrollbar
        sx={{
          height: '100%',
          '& .simplebar-content': {
            height: '100%'
          },
          '& .simplebar-scrollbar:before': {
            background: 'var(--nav-scrollbar-color)'
          }
        }}
      > */}
      <Stack sx={{ height: "100%" }}>
        <Stack alignItems="center" direction="row" spacing={2} sx={{ p: 3 }}>
          {/* <Box
              // component={NextLink}
              // href="#"
              // href={paths.index}
              sx={{
                borderColor: 'var(--nav-logo-border)',
                borderRadius: 1,
                borderStyle: 'solid',
                borderWidth: 1,
                display: 'flex',
                height: 40,
                p: '4px',
                width: 40
              }}
            >
            </Box> */}
          <Brand />
          {/* <TenantSwitch sx={{ flexGrow: 1 }} /> */}
        </Stack>
        <Stack
          component="nav"
          spacing={2}
          sx={{
            flexGrow: 1,
            px: 2,
          }}
        >
          {/* {sections.map((section, index) => (
              <SideNavSection
                items={section.items}
                key={index}
                // pathname={pathname}
                subheader={section.subheader}
              />
            ))} */}
        </Stack>
        {/* <Box sx={{ p: 3 }}>
            <Typography variant="subtitle1">
              Need help?
            </Typography>
            <Typography
              color="neutral.400"
              sx={{ mb: 2 }}
              variant="body2"
            >
              Please check our docs.
            </Typography>
            <Button
              component={NextLink}
              fullWidth
              href="#"
              // href={paths.docs.welcome}
              startIcon={(
                <SvgIcon>
                  <InsertDriveFileOutlinedIcon />
                </SvgIcon>
              )}
              variant="contained"
            >
              Documentation
            </Button>
          </Box> */}
      </Stack>
      {/* </Scrollbar> */}
    </Drawer>
  );
};

// SideNav.propTypes = {
//   color: PropTypes.oneOf(['blend-in', 'discreet', 'evident']),
//   sections: PropTypes.array
// };
