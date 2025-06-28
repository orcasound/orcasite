import Image from "next/image";

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
  const iconConfig = {
    BIOPHONY: {
      src: whaleFlukeIconImage.src,
      alt: "Whale fluke icon",
    },
    ANTHROPHONY: {
      src: vesselIconImage.src,
      alt: "Vessel icon",
    },
    GEOPHONY: {
      src: wavesIconImage.src,
      alt: "Waves icon",
    },
  }[audioCategory];

  return (
    <>
      {iconConfig && (
        <Image
          src={iconConfig.src}
          width={size ?? 20}
          height={size ?? 20}
          // {...(size ? { width: size, height: size } : { fill: true })}
          alt={iconConfig.alt}
          style={{
            display: "block",
            margin: "auto",
          }}
        />
      )}
    </>
  );
}
