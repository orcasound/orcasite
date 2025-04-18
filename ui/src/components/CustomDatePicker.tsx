// components/CustomDatePicker.tsx
import { Box, TextField } from "@mui/material";
import { Dayjs } from "dayjs";
import dayjs from "dayjs";
import React, { forwardRef } from "react";
import ReactDatePicker from "react-datepicker";

interface CustomDatePickerProps {
  valueProp: Dayjs | null;
  onDataChange: (name: string, value: Dayjs | null) => void;
  name: string;
  label?: string;
}

interface CustomInputProps {
  value?: string;
  onClick?: () => void;
  onChange?: (e: React.ChangeEvent<HTMLInputElement>) => void;
  label?: string;
}

// ForwardRef so react-datepicker can control focus
const CustomInput = forwardRef<HTMLInputElement, CustomInputProps>(
  ({ value, onClick, onChange, label }, ref) => (
    <TextField
      fullWidth
      size="small"
      variant="outlined"
      label={label}
      value={value}
      onClick={onClick}
      onChange={onChange}
      inputRef={ref}
    />
  ),
);

CustomInput.displayName = "CustomInput";

export const CustomDatePicker = ({
  valueProp,
  onDataChange,
  name,
  label,
}: CustomDatePickerProps) => {
  const selectedDate = valueProp ? valueProp.toDate() : null;

  const handleChange = (date: Date | null) => {
    onDataChange(name, date ? dayjs(date) : null);
  };

  return (
    <Box sx={{ flex: 1 }}>
      <ReactDatePicker
        selected={selectedDate}
        onChange={handleChange}
        customInput={<CustomInput label={label} />}
        dateFormat="yyyy-MM-dd"
        placeholderText="Select a date"
        popperPlacement="bottom-start"
      />
    </Box>
  );
};
