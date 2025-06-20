import { Theme } from "@mui/material";
import useMediaQuery from "@mui/material/useMediaQuery";
import {
  createContext,
  Dispatch,
  SetStateAction,
  useContext,
  useState,
} from "react";

type LayoutContextType = {
  alertOpen: boolean;
  setAlertOpen: Dispatch<SetStateAction<boolean>>;
  headerHeight: string;
  mobileMenuHeight: string;
  playbarExpanded: boolean;
  setPlaybarExpanded: Dispatch<SetStateAction<boolean>>;
  mobileTab: number;
  setMobileTab: Dispatch<SetStateAction<number>>;
  candidatePreview: boolean;
  setCandidatePreview: Dispatch<SetStateAction<boolean>>;
};

const LayoutContext = createContext<LayoutContextType | null>(null);

const alertHeight = "36px";
const desktopHeaderHeight = "64px";
const mobileHeaderHeight = "60px";
const mobileMenuHeight = "69px";

export const LayoutProvider = ({ children }: { children: React.ReactNode }) => {
  const [alertOpen, setAlertOpen] = useState<boolean>(true);

  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const noAlertHeight = smDown ? mobileHeaderHeight : desktopHeaderHeight;

  const headerHeight = alertOpen
    ? `calc(${alertHeight} + ${noAlertHeight})`
    : noAlertHeight;

  const [playbarExpanded, setPlaybarExpanded] = useState(false);

  const [candidatePreview, setCandidatePreview] = useState(true);

  // menuTab is the state of the mobile <MobileBottomNav>
  const [mobileTab, setMobileTab] = useState(0);

  return (
    <LayoutContext.Provider
      value={{
        alertOpen,
        setAlertOpen,
        headerHeight,
        mobileMenuHeight,
        playbarExpanded,
        setPlaybarExpanded,
        mobileTab,
        setMobileTab,
        candidatePreview,
        setCandidatePreview,
      }}
    >
      {children}
    </LayoutContext.Provider>
  );
};

export const useLayout = (): LayoutContextType => {
  const context = useContext(LayoutContext);
  if (!context) {
    throw new Error("useLayout must be used within a LayoutProvider");
  }
  return context;
};
