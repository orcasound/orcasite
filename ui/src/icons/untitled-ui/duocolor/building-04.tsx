import type { SVGProps } from "react";

const Building04 = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={24}
    height={24}
    fill="none"
    {...props}
  >
    <path fill="currentColor" d="M18 3H6v18h12V3Z" opacity={0.12} />
    <path
      stroke="currentColor"
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeWidth={2}
      d="M9.5 7h5m-5 4h5m-5 4h5m3.5 6V6.2c0-1.1201 0-1.6802-.218-2.108a1.9997 1.9997 0 0 0-.874-.874C16.4802 3 15.9201 3 14.8 3H9.2c-1.1201 0-1.6802 0-2.108.218a1.9999 1.9999 0 0 0-.874.874C6 4.5198 6 5.08 6 6.2V21m14 0H4"
    />
  </svg>
);

export default Building04;
