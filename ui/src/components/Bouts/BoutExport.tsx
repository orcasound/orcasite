import { Download } from "@mui/icons-material";
import {
  Box,
  CircularProgress,
  IconButton,
  Popover,
  Typography,
} from "@mui/material";
import JSZip from "jszip";
import { useRef, useState } from "react";

import { useBoutExportQuery } from "@/graphql/generated";

export default function BoutExport({ boutId }: { boutId: string }) {
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const buttonRef = useRef<HTMLButtonElement | null>(null);
  const timeoutRef = useRef<number | null>(null);
  const { refetch } = useBoutExportQuery({ boutId }, { enabled: false });

  const handleClick = () => {
    setIsLoading(true);
    refetch()
      .then(({ data }) => {
        const bout = data?.bout;
        if (bout) {
          const {
            exportJson,
            exportJsonFileName,
            exportScript,
            exportScriptFileName,
          } = bout;

          if (
            exportJson &&
            exportJsonFileName &&
            exportScript &&
            exportScriptFileName
          ) {
            const zip = new JSZip();
            zip.file(exportJsonFileName, exportJson);
            zip.file(exportScriptFileName, exportScript);
            zip.generateAsync({ type: "blob" }).then((content) => {
              download(`orcasound_${bout.id}.zip`, content);
            });

            if (buttonRef.current) {
              setAnchorEl(buttonRef.current);
              if (timeoutRef.current) {
                clearTimeout(timeoutRef.current);
              }
              timeoutRef.current = window.setTimeout(() => {
                setAnchorEl(null);
              }, 3000);
            }
          }
        }
      })
      .catch((error) => {
        console.error("Failed to export bout:", error);
      })
      .finally(() => {
        setIsLoading(false);
      });
  };

  const open = Boolean(anchorEl);

  const handleClose = () => {
    setAnchorEl(null);
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
    }
  };

  return (
    <Box
      display="flex"
      flexDirection="column"
      alignItems="center"
      minWidth={120}
    >
      <Box>
        <Typography variant="overline">Export bout</Typography>
      </Box>
      <Box>
        <IconButton
          onClick={handleClick}
          title="Export bout"
          ref={buttonRef}
          disabled={isLoading}
        >
          {isLoading ? <CircularProgress size={24} /> : <Download />}
        </IconButton>
      </Box>

      <Popover
        open={open}
        anchorEl={anchorEl}
        onClose={handleClose}
        anchorOrigin={{
          vertical: "bottom",
          horizontal: "center",
        }}
        transformOrigin={{
          vertical: "top",
          horizontal: "center",
        }}
        disableAutoFocus
        disableEnforceFocus
      >
        <Typography variant="subtitle2" sx={{ p: 2 }}>
          Downloaded bout export script
        </Typography>
      </Popover>
    </Box>
  );
}

// via: https://gist.github.com/Lehoczky/241f046b05c54af65918676888fc783f
function download(fileName: string, blob: Blob) {
  const downloadAnchor = document.createElement("a");

  downloadAnchor.download = fileName;
  downloadAnchor.href = URL.createObjectURL(blob);
  downloadAnchor.click();
  setTimeout(() => {
    URL.revokeObjectURL(downloadAnchor.href);
    downloadAnchor.remove();
  }, 200);
}
