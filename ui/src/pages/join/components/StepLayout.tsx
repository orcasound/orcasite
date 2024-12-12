import { Box, Container, Typography } from "@mui/material";
import Head from "next/head";

export const StepLayout = ({
  title,
  description,
  pageTitle,
  children,
}: {
  title: string;
  description?: string;
  pageTitle: string;
  children: React.ReactNode;
}) => {
  return (
    <>
      <Head>
        <title>{pageTitle} | Orcasound</title>
      </Head>

      <Container maxWidth="sm">
        <Box sx={{ mt: 4, mb: 6 }}>
          <Typography variant="h4" gutterBottom>
            {title}
          </Typography>
          {description && (
            <Typography variant="body1" color="text.secondary">
              {description}
            </Typography>
          )}
        </Box>
        {children}
      </Container>
    </>
  );
};
