import { RefObject, useEffect, useState } from "react";

export function useIsRelativeOverflow(
  containerRef: RefObject<HTMLElement | undefined>,
  targetRef: RefObject<HTMLElement | undefined>,
  callback?: (isOverflow: boolean) => void,
) {
  const [isOverflow, setIsOverflow] = useState<boolean>(false);

  const size = useWindowSize();

  useEffect(() => {
    const { current: currentContainer } = containerRef;
    const { current: currentTarget } = targetRef;

    const trigger = () => {
      setTimeout(() => {
        if (!currentContainer || !currentTarget) return;
        const hasOverflow =
          currentTarget.clientWidth > currentContainer.clientWidth;

        setIsOverflow(hasOverflow);

        if (callback) callback(hasOverflow);
      }, 100);
    };

    if (currentContainer && currentTarget) {
      trigger();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    callback,
    size.width,
    containerRef,
    targetRef,
    containerRef.current,
    targetRef.current,
  ]);

  return isOverflow;
}

function useWindowSize(): {
  height: number | undefined;
  width: number | undefined;
} {
  const isClient = typeof window === "object";

  function getSize(): {
    height: number | undefined;
    width: number | undefined;
  } {
    return {
      width: isClient ? window.innerWidth : undefined,
      height: isClient ? window.innerHeight : undefined,
    };
  }

  const [windowSize, setWindowSize] = useState(getSize);

  useEffect(() => {
    if (!isClient) {
      return;
    }

    function handleResize(): void {
      setWindowSize(getSize());
    }

    window.addEventListener("resize", handleResize);

    return (): void => window.removeEventListener("resize", handleResize);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []); // Empty array ensures that effect is only run on mount and unmount

  return windowSize;
}
