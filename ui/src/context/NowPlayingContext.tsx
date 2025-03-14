import React, { createContext, useContext, useState } from "react";

import { Candidate2 } from "@/types/DataTypes";

interface NowPlayingContextType {
  nowPlaying: Candidate2;
  setNowPlaying: React.Dispatch<React.SetStateAction<Candidate2>>;
}

const NowPlayingContext = createContext<NowPlayingContextType | undefined>(
  undefined,
);

export const NowPlayingProvider = ({
  children,
}: {
  children: React.ReactNode;
}) => {
  const [nowPlaying, setNowPlaying] = useState<Candidate2>({} as Candidate2);

  return (
    <NowPlayingContext.Provider value={{ nowPlaying, setNowPlaying }}>
      {children}
    </NowPlayingContext.Provider>
  );
};

export const useNowPlaying = (): NowPlayingContextType => {
  const context = useContext(NowPlayingContext);
  if (!context) {
    throw new Error("useNowPlaying must be used within a NowPlayingProvider");
  }
  return context;
};
