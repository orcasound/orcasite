import { useRouter } from "next/router";
import { useEffect } from "react";

const ModeratorIndexPage = () => {
  const router = useRouter();

  useEffect(() => {
    router.replace("/moderator/candidates");
  }, [router]);

  return null;
};

export default ModeratorIndexPage;
