import { Box, useMediaQuery, useTheme } from "@mui/material";
import type { StaticImageData } from "next/legacy/image";
import Image from "next/legacy/image";

export default function DetectionCategoryButton({
  icon,
  title,
}: {
  icon: StaticImageData;
  title: string;
}) {
  const theme = useTheme();
  const isDesktop = useMediaQuery(theme.breakpoints.up("sm"));
  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
      }}
    >
      <Image
        src={icon.src}
        alt={`${title} icon`}
        width={isDesktop ? 100 : 20}
        height={isDesktop ? 100 : 20}
      />
      {title}
    </Box>
  );
}
