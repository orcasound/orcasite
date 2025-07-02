import { Box } from "@mui/material";
import { styled } from "@mui/material/styles";

import Link from "@/components/Link";

export const FormContainer = styled(Box)(({ theme }) => ({
  maxWidth: 400,
  marginLeft: "auto",
  marginRight: "auto",
  display: "flex",
  flexDirection: "column",
  gap: theme.spacing(2),
}));

export const FormActionLink = styled(Link)(({ theme }) => ({
  color: theme.palette.primary.main,
  textDecoration: "none",
  "&:hover": {
    textDecoration: "underline",
  },
}));

FormActionLink.defaultProps = {
  variant: "body2",
};
