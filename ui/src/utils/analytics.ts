import ReactGA from "react-ga4";

export const GA_TRACKING_ID = process.env.NEXT_PUBLIC_GA_ID;

const about = {
  sampleAudioPlayed: (exampleTitle: string) =>
    sendEvent({
      category: "About",
      action: "Sample audio played",
      label: exampleTitle,
    }),
  learnTabClicked: () =>
    sendEvent({ category: "About", action: "Learn tab clicked" }),
  listenTabClicked: () =>
    sendEvent({ category: "About", action: "Listen tab clicked" }),
};

const detection = {
  dialogOpened: (feedSlug: string) =>
    sendEvent({
      category: "Detection",
      action: "Dialog opened",
      label: feedSlug,
    }),
  dialogClosed: (feedSlug: string) =>
    sendEvent({
      category: "Detection",
      action: "Dialog closed",
      label: feedSlug,
    }),
  submitted: (feedSlug: string) =>
    sendEvent({
      category: "Detection",
      action: "Detection submitted",
      label: feedSlug,
    }),
};

const stream = {
  started: (feedSlug: string) =>
    sendEvent({
      category: "Stream",
      action: "Player started",
      label: feedSlug,
    }),
  paused: (feedSlug: string) =>
    sendEvent({
      category: "Stream",
      action: "Player paused",
      label: feedSlug,
    }),
  error: (feedSlug: string) =>
    sendEvent({
      category: "Stream",
      action: "Player errored",
      label: feedSlug,
    }),
  playerTextClicked: (playerText: string) => {
    sendEvent({
      category: "Stream",
      action: "Player text clicked",
      label: playerText,
    });
  },
};

function sendEvent(...eventParams: Parameters<typeof ReactGA.event>) {
  if (GA_TRACKING_ID) {
    ReactGA.initialize(GA_TRACKING_ID);
    ReactGA.event(...eventParams);
  }
}

const nav = {
  logoClicked: () =>
    sendEvent({ category: "Navigation", action: "Home logo clicked" }),
  aboutTabClicked: () =>
    sendEvent({ category: "Navigation", action: "About tab clicked" }),
  listenTabClicked: () =>
    sendEvent({ category: "Navigation", action: "Listen tab clicked" }),
  feedbackTabClicked: () =>
    sendEvent({ category: "Navigation", action: "Feedback tab clicked" }),
  notificationsClicked: () =>
    sendEvent({ category: "Navigation", action: "Notifications clicked" }),
  feedSelected: (feedName: string) =>
    sendEvent({
      category: "Navigation",
      action: "Feed selected",
      label: feedName,
    }),
};

const form = {
  notificationSignupClicked: () =>
    sendEvent({ category: "Form", action: "Notification signup clicked" }),
  feedbackButtonClicked: () =>
    sendEvent({ category: "Form", action: "Feedback button clicked" }),
};

const reports = {
  reportOpened: (candidateId: string) =>
    sendEvent({
      category: "Reports",
      action: "Report opened",
      label: candidateId,
    }),
  reportAudioPlayed: (candidateId: string) =>
    sendEvent({
      category: "Reports",
      action: "Report audio played",
      label: candidateId,
    }),
};

export const analytics = {
  about,
  detection,
  stream,
  nav,
  form,
  reports,
};
