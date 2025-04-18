import SearchIcon from "@mui/icons-material/Search";
import Box from "@mui/material/Box";
import FilledInput from "@mui/material/FilledInput";
import FormControl from "@mui/material/FormControl";
import FormHelperText from "@mui/material/FormHelperText";
import IconButton from "@mui/material/IconButton";
import Input from "@mui/material/Input";
import InputAdornment from "@mui/material/InputAdornment";
import OutlinedInput from "@mui/material/OutlinedInput";
import TextField from "@mui/material/TextField";
import React, { FC } from "react";

type SearchBarProps = {
  setSearchQuery: (query: string) => void;
};

const SearchBar: FC<SearchBarProps> = ({ setSearchQuery }) => {
  const handleInput = (e: React.ChangeEvent<HTMLInputElement>) => {
    setSearchQuery(e.target.value.toLowerCase());
  };

  return (
    <div style={{ width: "100%" }}>
      {/* MAIN VISIBLE SEARCH FIELD */}
      <Box>
        <FormControl sx={{ width: "100%" }} variant="outlined">
          <OutlinedInput
            id="outlined-adornment-password"
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

      {/* HIDDEN ALTERNATIVE VARIANTS (for future reference or toggling) */}

      <Box
        sx={{
          display: "none",
          justifyContent: "flex-end",
          alignItems: "flex-end",
          height: "42px",
          overflow: "hidden",
          border: "1px solid",
          borderRadius: "100px",
          paddingBotom: "10px",
        }}
      >
        <FormControl sx={{ width: "100%" }} variant="filled">
          <FilledInput
            id="outlined-adornment-password"
            placeholder="Search report descriptions..."
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

      <form style={{ width: "100%", display: "none", alignItems: "center" }}>
        <TextField
          id="search-bar"
          className="text"
          onInput={handleInput}
          label={
            <Box style={{ width: "100%", display: "flex" }}>
              <IconButton type="submit" aria-label="search">
                <SearchIcon style={{ fill: "black" }} />
              </IconButton>
              Search report descriptions
            </Box>
          }
          variant="filled"
          placeholder="Search..."
          size="medium"
          fullWidth
        />
      </form>

      <FormControl sx={{ display: "none", width: "100%" }}>
        <FilledInput
          placeholder="Search report descriptions"
          startAdornment={
            <InputAdornment position="start">
              <IconButton edge="start">
                <SearchIcon />
              </IconButton>
            </InputAdornment>
          }
        />
      </FormControl>

      <FormControl
        variant="standard"
        sx={{ m: 1, mt: 3, width: "25ch", display: "none" }}
      >
        <Input
          id="standard-adornment-weight"
          placeholder="Search reports..."
          startAdornment={
            <InputAdornment position="start">
              <SearchIcon />
            </InputAdornment>
          }
          aria-describedby="standard-weight-helper-text"
          inputProps={{
            "aria-label": "weight",
          }}
        />
        <FormHelperText id="standard-weight-helper-text"></FormHelperText>
      </FormControl>

      <FormControl
        sx={{ m: 1, width: "25ch", display: "none" }}
        variant="filled"
      >
        <FilledInput
          id="outlined-adornment-weight"
          placeholder="Search reports..."
          startAdornment={
            <InputAdornment position="start">
              <SearchIcon />
            </InputAdornment>
          }
          aria-describedby="outlined-weight-helper-text"
          inputProps={{
            "aria-label": "weight",
          }}
        />
      </FormControl>
    </div>
  );
};

export default SearchBar;
