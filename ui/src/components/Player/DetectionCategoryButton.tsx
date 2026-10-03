import { Box } from "@mui/material";
import type { StaticImageData } from "next/image";
import Image from "next/image";

export default function DetectionCategoryButton({
  icon,
  title,
}: {
  icon: StaticImageData;
  title: string;
}) {
  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        alignItems: "center",
      }}
    >
      <Box sx={{ display: { xs: "block", sm: "none" } }}>
        <Image src={icon.src} alt={`${title} icon`} width={20} height={20} />
      </Box>
      <Box sx={{ display: { xs: "none", sm: "block" } }}>
        <Image src={icon.src} alt={`${title} icon`} width={100} height={100} />
      </Box>
      {title}
    </Box>
  );
}
