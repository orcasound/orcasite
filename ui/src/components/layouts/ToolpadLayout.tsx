"use client";
import DashboardIcon from "@mui/icons-material/Dashboard";
import type { Navigation } from "@toolpad/core/AppProvider";
import { DashboardLayout as ToolpadDashboardLayout } from "@toolpad/core/DashboardLayout";
import { NextAppProvider } from "@toolpad/core/nextjs";
import { PageContainer } from "@toolpad/core/PageContainer";
import * as React from "react";
import { ReactElement } from "react";

const NAVIGATION: Navigation = [
  {
    kind: "header",
    title: "Main items",
  },
  {
    segment: "",
    title: "Dashboard",
    icon: <DashboardIcon />,
  },
];

const BRANDING = {
  title: "My Toolpad Core App",
};

const ToolpadLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <NextAppProvider>
      <ToolpadDashboardLayout navigation={NAVIGATION} branding={BRANDING}>
        <PageContainer>{children}</PageContainer>
      </ToolpadDashboardLayout>
    </NextAppProvider>
  );
};

export function getToolpadLayout(page: ReactElement) {
  return <ToolpadLayout>{page}</ToolpadLayout>;
}
