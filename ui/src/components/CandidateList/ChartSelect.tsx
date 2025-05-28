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
  fontSize?: string | number;
  onChange: (event: SelectChangeEvent<unknown>) => void;
  minWidth?: number;
  sx?: SxProps;
};

export default function ChartSelect({
  name,
  value,
  list,
  size = "small",
  variant,
  fontSize,
  onChange,
  minWidth = 100,
}: SelectProps) {
  return (
    <Box sx={{ minWidth: minWidth, flex: 1 }}>
      <FormControl fullWidth size={size} variant={variant}>
        <Select
          id={`${name}-select`}
          value={value}
          size={size}
          onChange={onChange}
          name={name}
          sx={{
            "& .MuiSelect-select": {
              padding: "3px 9px", // match small button padding
              fontSize: fontSize ?? "13px", // match small button font
              minHeight: "auto",
              display: "flex",
              alignItems: "center",
              border: "1px solid rgba(255,255,255,.25)",
            },
            // Optional: adjust icon size
            "& .MuiSelect-icon": {
              fontSize: "13px",
            },
            ...(variant === "standard" && {
              "&::before": {
                borderBottom: "none !important", // removes underline in standard variant
              },
              "&::after": {
                borderBottom: "none !important", // removes focused underline
              },
              "&:hover:not(.Mui-disabled)::before": {
                borderBottom: "none",
              },
              "& .MuiSelect-select": {
                padding: 0,
                fontSize: { fontSize },
              },
            }),
          }}
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
