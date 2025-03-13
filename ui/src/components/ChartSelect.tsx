import { InputLabel } from "@mui/material";
import Box from "@mui/material/Box";
import FormControl from "@mui/material/FormControl";
import MenuItem from "@mui/material/MenuItem";
import Select, { SelectChangeEvent } from "@mui/material/Select";
import * as React from "react";

type SelectProps = {
  name: string;
  value: string | number;
  list: { label: string; value: string | number }[];
  size?: "small" | "medium";
  onChange: (event: SelectChangeEvent<unknown>) => void;
};

export default function ChartSelect({
  name,
  value,
  list,
  size,
  onChange,
}: SelectProps) {
  return (
    <Box sx={{ minWidth: 120, flex: 1 }}>
      <FormControl fullWidth>
        {/* removing the InputLabel to make Select appearance match designs, will revist for accessibility */}
        <InputLabel
          style={{ display: "none" }}
          id={`chart-select-${name}-label`}
        >
          {name}
        </InputLabel>
        <Select
          id={`${name}-select`}
          value={value}
          size={size}
          onChange={onChange}
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
