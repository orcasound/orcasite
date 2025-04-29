import React, { createContext, useContext } from "react";

import { Dataset } from "@/types/DataTypes";

const DataContext = createContext<Dataset>({
  human: [],
  ai: [],
  combined: [],
  isSuccess: false,
  feeds: [],
});

export const useData = () => useContext(DataContext);

export const DataProvider = ({
  children,
  data,
}: {
  children: React.ReactNode;
  data: Dataset;
}) => {
  return <DataContext.Provider value={data}>{children}</DataContext.Provider>;
};
