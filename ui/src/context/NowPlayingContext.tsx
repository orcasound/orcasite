import React, { createContext, useContext, useRef, useState } from "react";

import { VideoJSPlayer } from "@/components/Player/VideoJS";
import { Candidate } from "@/types/DataTypes";

interface NowPlayingContextType {
  nowPlaying: Candidate;
  setNowPlaying: React.Dispatch<React.SetStateAction<Candidate>>;
  masterPlayerRef: React.MutableRefObject<VideoJSPlayer | null>;
  masterPlayerStatus: string;
  setMasterPlayerStatus: React.Dispatch<React.SetStateAction<string>>;
}

const NowPlayingContext = createContext<NowPlayingContextType | undefined>(
  undefined,
);

export const NowPlayingProvider = ({
  children,
}: {
  children: React.ReactNode;
}) => {
  const [nowPlaying, setNowPlaying] = useState<Candidate>({} as Candidate);
  const [masterPlayerStatus, setMasterPlayerStatus] = useState("empty");
  const masterPlayerRef = useRef<VideoJSPlayer | null>(null);

  return (
    <NowPlayingContext.Provider
      value={{
        nowPlaying,
        setNowPlaying,
        masterPlayerRef,
        masterPlayerStatus,
        setMasterPlayerStatus,
      }}
    >
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
