import * as React from "react";

function SvgPlayIcon(props) {
  return (
    <svg width="1em" height="1em" viewBox="0 0 18 18" fill="none" {...props}>
      <path
        d="M12.864 8.232a.7.7 0 010 1.135L8.383 12.6a.7.7 0 01-1.109-.568V5.567A.7.7 0 018.385 5l4.479 3.232z"
        fill="#013C74"
      />
      <rect
        x={1.65}
        y={1.05}
        width={15.5}
        height={15.5}
        rx={7.75}
        stroke="#013C74"
        strokeWidth={1.5}
      />
    </svg>
  );
}

export default SvgPlayIcon;
