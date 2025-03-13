import { subDays, subHours } from "date-fns";

export interface Notification {
  id: string;
  author?: string;
  avatar?: string;
  company?: string;
  createdAt: number;
  description?: string;
  job?: string;
  title?: string;
  read?: boolean;
  type: string;
}

const now = new Date();

export const notifications: Notification[] = [
  {
    id: "5e8883f1b51cc1956a5a1ec0",
    author: "Jie Yang Song",
    avatar: "/assets/avatars/avatar-jie-yan-song.png",
    createdAt: subHours(now, 2).getTime(),
    job: "Remote React / React Native Developer",
    read: true,
    type: "job_add",
  },
  {
    id: "bfb21a370c017acc416757c7",
    author: "Jie Yang Song",
    avatar: "/assets/avatars/avatar-jie-yan-song.png",
    createdAt: subHours(now, 2).getTime(),
    job: "Senior Golang Backend Engineer",
    read: false,
    type: "job_add",
  },
  {
    id: "20d9df4f23fff19668d7031c",
    createdAt: subDays(now, 1).getTime(),
    description: "Logistics management is now available",
    read: true,
    type: "new_feature",
  },
  {
    id: "5e8883fca0e8612044248ecf",
    author: "Jie Yang Song",
    avatar: "/assets/avatars/avatar-jie-yan-song.png",
    company: "Augmastic Inc",
    createdAt: subHours(now, 2).getTime(),
    read: false,
    type: "company_created",
  },
];
