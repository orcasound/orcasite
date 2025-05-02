import SearchIcon from "@mui/icons-material/Search";
import Box from "@mui/material/Box";
import FormControl from "@mui/material/FormControl";
import IconButton from "@mui/material/IconButton";
import InputAdornment from "@mui/material/InputAdornment";
import OutlinedInput from "@mui/material/OutlinedInput";
import React from "react";

import { useData } from "@/context/DataContext";

type SearchBarProps = {
  setSearchQuery: (query: string) => void;
};

const SearchBar = () => {
  const { setFilters } = useData();
  const handleInput = (e: React.ChangeEvent<HTMLInputElement>) => {
    setFilters((prevFilters) => ({
      ...prevFilters,
      searchQuery: e.target.value.toLowerCase(),
    }));
  };

  return (
    <div style={{ width: "100%" }}>
      <Box>
        <FormControl sx={{ width: "100%" }} variant="outlined">
          <OutlinedInput
            id="search-bar-input"
            onInput={handleInput}
            placeholder="Search report descriptions..."
            size="small"
            startAdornment={
              <InputAdornment position="start">
                <IconButton aria-label="search" edge="start">
                  <SearchIcon />
                </IconButton>
              </InputAdornment>
            }
          />
        </FormControl>
      </Box>
    </div>
  );
};

export default SearchBar;
