import { Theme, useMediaQuery } from "@mui/material";

import CandidatesGrid from "@/components/CandidateList/CandidatesGrid";
import DesktopTabs from "@/components/CandidateList/DesktopTabs";
import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import { LayoutContext } from "@/context/LayoutContext";
import type { NextPageWithLayout } from "@/pages/_app";

const ModeratorCandidatesPage: NextPageWithLayout = () => {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  return (
    <LayoutContext.Provider value="leftNav">
      {mdDown ? <DesktopTabs tabIndex={0} /> : <CandidatesGrid />}
    </LayoutContext.Provider>
  );
};

ModeratorCandidatesPage.getLayout = getLeftNavLayout;

export default ModeratorCandidatesPage;
