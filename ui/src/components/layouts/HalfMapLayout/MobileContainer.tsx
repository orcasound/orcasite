import { Container } from "@mui/material";

type MobileContainerProps = {
  children: React.ReactNode;
};

export const MobileContainer = ({ children }: MobileContainerProps) => {
  return (
    <Container
      maxWidth="xl"
      className={"mobile-container"}
      sx={{
        py: 2,
        px: 1.5,
        flex: 1,
        display: "flex",
        flexDirection: "column",
        overflow: "hidden", // prevents double scrollbars
      }}
    >
      {children}
    </Container>
  );
};
