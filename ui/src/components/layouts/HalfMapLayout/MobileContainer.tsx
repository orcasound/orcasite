import { Container } from "@mui/material";

type MobileContainerProps = {
  children: React.ReactNode;
};

export const MobileContainer = ({ children }: MobileContainerProps) => {
  return (
    <Container
      maxWidth="xl"
      sx={{
        py: 2,
        px: 1.5,
        height: "100%",
        overflowY: "auto",
      }}
    >
      {children}
    </Container>
  );
};
