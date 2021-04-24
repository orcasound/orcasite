import ReactGA from "react-ga"

const about = {
  sampleAudioPlayed: (exampleTitle) =>
    ReactGA.event({
      category: "About",
      action: "Sample audio played",
      label: exampleTitle,
    }),
}

const detection = {
  dialogOpened: (feedSlug) =>
    ReactGA.event({
      category: "Detection",
      action: "Dialog opened",
      label: feedSlug,
    }),
  dialogClosed: (feedSlug) =>
    ReactGA.event({
      category: "Detection",
      action: "Dialog closed",
      label: feedSlug,
    }),
  submitted: (feedSlug) =>
    ReactGA.event({
      category: "Detection",
      action: "Detection submitted",
      label: feedSlug,
    }),
}

const stream = {
  started: (feedSlug) =>
    ReactGA.event({
      category: "Stream",
      action: "Player started",
      label: feedSlug,
    }),
  paused: (feedSlug) =>
    ReactGA.event({
      category: "Stream",
      action: "Player paused",
      label: feedSlug,
    }),
}

const nav = {
  logoClicked: () =>
    ReactGA.event({ category: "Navigation", action: "Home logo clicked" }),
  aboutTabClicked: () =>
    ReactGA.event({ category: "Navigation", action: "About tab clicked" }),
  listenTabClicked: () =>
    ReactGA.event({ category: "Navigation", action: "Listen tab clicked" }),
  feedSelected: (feedName) => ReactGA.event({
      category: "Navigation",
      action: "Feed selected",
      label: feedName,
    }),
}

const form = {
  notificationSignupClicked: () =>
    ReactGA.event({ category: "Form", action: "Notification signup clicked" }),
  feedbackButtonClicked: () =>
    ReactGA.event({ category: "Form", action: "Feedback button clicked" }),
}

export default {
  about,
  detection,
  stream,
  nav,
  form,
}
