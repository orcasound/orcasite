import Box from "@mui/material/Box";
import FormControl from "@mui/material/FormControl";
import MenuItem from "@mui/material/MenuItem";
import Select, { SelectChangeEvent } from "@mui/material/Select";
import { SxProps } from "@mui/system";
import * as React from "react";

type SelectProps = {
  name: string;
  value: string | number;
  list: { label: string; value: string | number }[];
  size?: "small" | "medium";
  variant?: "standard" | "outlined" | "filled";
  onChange: (event: SelectChangeEvent<unknown>) => void;
  sx?: SxProps;
};

export default function ChartSelect({
  name,
  value,
  list,
  size = "small",
  variant,
  onChange,
  sx,
}: SelectProps) {
  return (
    <Box sx={{ minWidth: 120, flex: 1 }}>
      <FormControl fullWidth size={size}>
        <Select
          id={`${name}-select`}
          value={value}
          // size={size}
          variant={variant}
          onChange={onChange}
          name={name}
          sx={sx}
        >
          {list?.map((el) => (
            <MenuItem key={el.label} value={el.value}>
              {el.label}
            </MenuItem>
          ))}
        </Select>
      </FormControl>
    </Box>
  );
}
