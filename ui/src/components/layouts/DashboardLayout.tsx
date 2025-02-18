"use client";
import DashboardIcon from "@mui/icons-material/Dashboard";
import type { Navigation } from "@toolpad/core/AppProvider";
import { DashboardLayout as ToolpadDashboardLayout } from "@toolpad/core/DashboardLayout";
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

const DashboardLayout = ({ children }: ReactElement) => {
  return (
    <ToolpadDashboardLayout navigation={NAVIGATION} branding={BRANDING}>
      <PageContainer>{children}</PageContainer>
    </ToolpadDashboardLayout>
  );
};

export function getDashboardLayout(page: ReactElement) {
  return <DashboardLayout>{page}</DashboardLayout>;
}
