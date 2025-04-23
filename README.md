# Orcasound Dashboard Prototype

This is a UX and engineering contribution to Orcasound, an open-source platform that streams real-time underwater audio from hydrophones in the Salish Sea to detect and protect orcas.

I’ve been volunteering with the Orcasound team since 2023, focused on improving the experience for listeners, scientists, and volunteers who report orca detections. This prototype reimagines the reporting interface and proposes a scalable, data-rich dashboard for reviewing past detections, integrating AI models, and surfacing patterns that inform conservation.

**Live prototype:** [https://orcasound-dashboard.vercel.app/moderator](https://orcasound-dashboard.vercel.app/moderator)

**Tech stack:** Next.js, React, Typescript, Material UI 

## Designs in progress

Before -- the existing app looks like this. On the left is a list of hydrophone locations. Users can stream the audio live, and submit a report when they hear an interesting sound such as a whale, ship, or other. On the right is a list of user-submitted reports.

![Before screens](https://github.com/user-attachments/assets/86dc0f36-38c0-4625-85c7-efa8a25c39ad)


After -- in my redesign, I focused on resolving the following user pain points:
* Difficult to navigate from the home screen to the Reports page.
* No filter/search functions.
* The way it groups reports into "candidates" creates disjointed segments of audio.
* Doesn't pull relevant data from other sources.
Users need to quickly see where periods of activity are, so they can find and hear whale sounds. 

Redesigns

![After screens](https://github.com/user-attachments/assets/df3c6e68-2f46-46ec-b443-25d82bf5e606)
