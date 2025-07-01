import { TablePagination } from "@mui/material";
import { ElementType } from "react";

interface TablePaginationWrapperProps {
  count: number;
  page: number;
  rowsPerPage: number;
  onPageChange: (event: unknown, newPage: number) => void;
  onRowsPerPageChange: (event: React.ChangeEvent<HTMLInputElement>) => void;
  rowsPerPageOptions?: number[];
  component?: ElementType;
  sx?: object;
  showBottomBorder?: boolean;
}

const TablePaginationWrapper = ({
  count,
  page,
  rowsPerPage,
  onPageChange,
  onRowsPerPageChange,
  rowsPerPageOptions = [10, 50, 100, 1000],
  component = "div",
  sx = {},
  showBottomBorder = false,
}: TablePaginationWrapperProps) => {
  const defaultSx = showBottomBorder
    ? {
        borderBottom: 1,
        borderColor: "divider",
        display: "flex",
        justifyContent: "flex-end",
        alignItems: "center",
        ...sx,
      }
    : sx;

  return (
    <TablePagination
      component={component}
      count={count}
      page={page}
      rowsPerPage={rowsPerPage}
      onPageChange={onPageChange}
      onRowsPerPageChange={onRowsPerPageChange}
      rowsPerPageOptions={rowsPerPageOptions}
      sx={defaultSx}
    />
  );
};

export default TablePaginationWrapper;
