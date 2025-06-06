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
};

const LayoutContext = createContext<LayoutContextType | null>(null);

const alertHeight = "36px";
const desktopHeaderHeight = "64px";
const mobileMenuHeight = "69px";

export const LayoutProvider = ({ children }: { children: React.ReactNode }) => {
  const [alertOpen, setAlertOpen] = useState<boolean>(true);

  const headerHeight = alertOpen
    ? `calc(${alertHeight} + ${desktopHeaderHeight})`
    : desktopHeaderHeight;

  return (
    <LayoutContext.Provider
      value={{
        alertOpen,
        setAlertOpen,
        headerHeight,
        mobileMenuHeight,
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
