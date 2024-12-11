import { Box } from "@mui/material";
import { useEffect, useRef, useState } from "react";

export default function SpectrogramTimeline({
  playerTime,
}: {
  playerTime?: Date;
}) {
  const spectrogramContainer = useRef<HTMLDivElement | null>(null);
  const [spectrogramTime, setSpectrogramTime] = useState<Date>();
  const [isDragging, setIsDragging] = useState<boolean>(false);
  const [zoomLevel, setZoomLevel] = useState<number>(10);

  const visibleContainerStartX = useRef<number>(0);
  const containerScrollX = useRef<number>(0);

  const pixelsPerMinute = 50 * zoomLevel;

  useEffect(() => {
    if (spectrogramTime === undefined && playerTime !== undefined) {
      setSpectrogramTime(playerTime);
    }
  }, [playerTime, spectrogramTime]);

  const handleTouchStart = (e: React.TouchEvent) => {
    setIsDragging(true);
    visibleContainerStartX.current =
      e.touches[0].pageX - (spectrogramContainer.current?.offsetLeft ?? 0);
    containerScrollX.current = spectrogramContainer.current?.scrollLeft ?? 0;
  };

  const handleTouchMove = (e: React.TouchEvent) => {
    if (!isDragging || !spectrogramContainer.current) return;
    e.preventDefault();
    const containerCursorX =
      e.touches[0].pageX - spectrogramContainer.current.offsetLeft;
    const move = containerCursorX - visibleContainerStartX.current;
    spectrogramContainer.current.scrollLeft = containerScrollX.current - move;
  };

  const handleMouseDown = (e: React.MouseEvent) => {
    setIsDragging(true);
    visibleContainerStartX.current =
      e.pageX - (spectrogramContainer.current?.offsetLeft ?? 0);
    containerScrollX.current = spectrogramContainer.current?.scrollLeft ?? 0;
  };

  const handleMouseLeave = () => {
    setIsDragging(false);
  };

  const handleMouseUp = () => {
    setIsDragging(false);
  };

  const handleMouseMove = (e: React.MouseEvent) => {
    if (!isDragging || !spectrogramContainer.current) return;
    e.preventDefault();
    const containerCursorX = e.pageX - spectrogramContainer.current.offsetLeft;
    const move = containerCursorX - visibleContainerStartX.current;
    spectrogramContainer.current.scrollLeft = containerScrollX.current - move;
  };

  const handleWheel = (e: React.WheelEvent) => {
    e.preventDefault();
    setZoomLevel((zoom) => Math.min(Math.max(5, zoom + e.deltaY * -0.01), 15));
    // if (!spectrogramContainer.current) return;
    // spectrogramContainer.current.scrollLeft -= e.deltaY;
  };

  useEffect(() => {
    const container = spectrogramContainer.current;
    if (container) {
      container.addEventListener("mouseleave", handleMouseLeave);
      container.addEventListener("mouseup", handleMouseUp);
      return () => {
        container.removeEventListener("mouseleave", handleMouseLeave);
        container.removeEventListener("mouseup", handleMouseUp);
      };
    }
  }, []);

  return (
    <>
      {zoomLevel}
      <Box
        ref={spectrogramContainer}
        minHeight={200}
        bgcolor="#efefef"
        width={"100%"}
        display="flex"
        sx={{
          overflowX: "hidden",
          msOverflowStyle: "none",
          "&::-webkit-scrollbar": { display: "none", height: 0 },
          scrollbarWidth: "none",
          cursor: isDragging ? "grabbing" : "grab",
          userSelect: "none",
        }}
        onMouseDown={handleMouseDown}
        onMouseMove={handleMouseMove}
        onTouchStart={handleTouchStart}
        onTouchMove={handleTouchMove}
        onWheel={handleWheel}
      >
        {Array(10)
          .fill(0)
          .map((_, idx) => (
            <Box
              key={idx}
              bgcolor={"#ccc"}
              sx={{ minWidth: pixelsPerMinute, minHeight: "100%" }}
              display="flex"
              alignItems="center"
              justifyContent="center"
              borderRight="1px solid #eee"
            >
              spectrogram {idx}
            </Box>
          ))}
      </Box>
      ({JSON.stringify(playerTime)})
    </>
  );
}
