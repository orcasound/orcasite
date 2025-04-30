import React, {
  createContext,
  useCallback,
  useContext,
  useRef,
  useState,
} from "react";

import { VideoJSPlayer } from "@/components/Player/VideoJS";
import { Candidate } from "@/types/DataTypes";

interface NowPlayingContextType {
  nowPlaying: Candidate;
  setNowPlaying: React.Dispatch<React.SetStateAction<Candidate>>;
  masterPlayerRef: React.MutableRefObject<VideoJSPlayer | null>;
  masterPlayerStatus: string;
  setMasterPlayerStatus: React.Dispatch<React.SetStateAction<string>>;
  queue: Candidate[];
  setQueue: React.Dispatch<React.SetStateAction<Candidate[]>>;
  onPlayerEnd: () => void;
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
  const [queue, setQueue] = useState<Candidate[]>([]);
  const onPlayerEnd = useCallback(() => {
    const currentIndex = queue.indexOf(nowPlaying);
    const nextIndex = currentIndex + 1;
    setNowPlaying(queue[nextIndex]);
    console.log("currentIndex in queue: " + currentIndex);
  }, [queue, nowPlaying, setNowPlaying]);
  return (
    <NowPlayingContext.Provider
      value={{
        nowPlaying,
        setNowPlaying,
        masterPlayerRef,
        masterPlayerStatus,
        setMasterPlayerStatus,
        queue,
        setQueue,
        onPlayerEnd,
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
