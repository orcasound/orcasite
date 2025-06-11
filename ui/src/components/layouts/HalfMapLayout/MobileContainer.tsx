import { Container } from "@mui/material";

type MobileContainerProps = {
  children: React.ReactNode;
};

export const MobileContainer = ({ children }: MobileContainerProps) => {
  return (
    <Container
      maxWidth="xl"
      id={"mobile-container"}
      sx={{
        py: 2,
        px: 1.5,
        flex: 1,
        display: "flex",
        flexDirection: "column",
        pb: 6,
      }}
    >
      {children}
    </Container>
  );
};
