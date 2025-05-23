// import SearchMdIcon from "icons-react/build/esm/SearchMd";
import SearchIcon from "@mui/icons-material/Search";
import { IconButton, SvgIcon, Tooltip } from "@mui/material";
import type { FC } from "react";
import { useCallback, useState } from "react";

import { SearchDialog } from "./search-dialog";

export const SearchButton: FC = () => {
  const [openDialog, setOpenDialog] = useState<boolean>(false);

  const handleOpen = useCallback((): void => {
    setOpenDialog(true);
  }, []);

  const handleClose = useCallback((): void => {
    setOpenDialog(false);
  }, []);

  return (
    <>
      <Tooltip title="Search">
        <IconButton onClick={handleOpen}>
          <SvgIcon>
            <SearchIcon />
          </SvgIcon>
        </IconButton>
      </Tooltip>
      <SearchDialog onClose={handleClose} open={openDialog} />
    </>
  );
};
