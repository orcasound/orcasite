import Image from "next/legacy/image";

import { AudioCategory } from "@/graphql/generated";
import vesselIconImage from "@/public/icons/vessel-purple.svg";
import wavesIconImage from "@/public/icons/water-waves-blue.svg";
import whaleFlukeIconImage from "@/public/icons/whale-fluke-gray.svg";

export default function CategoryIcon({
  audioCategory,
  size,
}: {
  audioCategory: AudioCategory;
  size?: number;
}) {
  size = size ?? 15;
  if (audioCategory === "BIOPHONY")
    return (
      <Image
        src={whaleFlukeIconImage.src}
        width={size}
        height={size}
        alt="Whale fluke icon"
        title="Biophony"
      />
    );
  if (audioCategory === "ANTHROPHONY")
    return (
      <Image
        src={vesselIconImage.src}
        width={size}
        height={size}
        alt="Vessel icon"
        title="Anthrophony"
      />
    );
  if (audioCategory === "GEOPHONY")
    return (
      <Image
        src={wavesIconImage.src}
        width={size}
        height={size}
        alt="Waves icon"
        title="Geophony"
      />
    );
}
