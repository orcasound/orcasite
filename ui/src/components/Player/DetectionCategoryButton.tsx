import { Box } from "@mui/material";
import type { StaticImageData } from "next/legacy/image";
import Image from "next/legacy/image";

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
      <Image src={icon.src} alt={`${title} icon`} width={100} height={100} />
      {title}
    </Box>
  );
}
