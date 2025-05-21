import { createContext, useContext } from "react";

type LayoutType = "halfMap" | "leftNav";

export const LayoutContext = createContext<LayoutType | null>(null);

export const useLayout = () => useContext(LayoutContext);
