import Box from "@mui/material/Box";
import FormControl from "@mui/material/FormControl";
import MenuItem from "@mui/material/MenuItem";
import Select from "@mui/material/Select";
import { SelectChangeEvent } from "@mui/material/Select";
import * as React from "react";
import { useState } from "react";

interface SelectProps {
  name: string;
  value: string | number;
  onChange: (event: SelectChangeEvent<HTMLSelectElement>) => void;
  list?: { label: string; value: string | number }[];
  size?: string;
}

const ChartSelect: React.FC<SelectProps> = ({
  onChange,
  name,
  value,
  list,
  size = "small",
}) => {
  const [selectedValue, setSelectedValue] = useState<string | number>(value);

  return (
    <Box sx={{ flex: 1, minWidth: "200px" }}>
      <FormControl fullWidth>
        <Select
          labelId="demo-simple-select-label"
          id="demo-simple-select"
          value={selectedValue}
          onChange={onChange}
          name={name}
          size={size}
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
};

export default ChartSelect;
