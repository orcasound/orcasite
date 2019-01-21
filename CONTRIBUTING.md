# Contributing to Orcasound

Welcome to Orcasound! We're thrilled you're interested in contributing to the site.

### General workflow
We recommend following these steps if you'd like to contribute to the repo.

0. Check out the [Trello board](https://trello.com/b/wBg0qhss/orcasound-roadmap) for an overview of features we're considering
1. Fork the Orcasite repo
2. Develop on master with feature flags (using ENV variables as found in `/assets/webpack.config.js` for frontend)
  	- See [this article](https://devops.com/feature-branching-vs-feature-flags-whats-right-tool-job/) for reasoning
3. Submit a PR (don't review your own)!
  	- To maintain a consistent style, we recommend running [Prettier](https://github.com/prettier/prettier) on js, and `mix format` before submission
4. Once a PR is merged, we can throw it up onto the dev server to see how things look!

### Questions?
If you have any questions about parts of the architecture and site, these are contributors with context about various parts:

- Paul Cretu - Orcanode, streaming backend, ffmpeg
- Skander Mzali - Elixir backend, JS/React frontend

### Learning resources
If you're new to Elixir (or JS and React), that's no problem! Here are some resources for learning what they're about.

#### Elixir

- [Official Elixir Documentation](https://elixir-lang.org/getting-started/introduction.html) - An excellent place to get started.
- [Elixir Forum](https://elixirforum.com/) - A huge repository of discussion and answered questions
- [Elixir Slack](https://elixir-slackin.herokuapp.com/) - Super friendly bunch of people
- [Absinthe](https://hexdocs.pm/absinthe/overview.html) - Elixir library for GraphQL

##### Some cool videos

- [Micropatterns in Elixir](https://www.youtube.com/watch?v=9uvp4h7gXHg) - Great video on how to think about recursion to solve problems
- [An intro to OTP](https://www.youtube.com/watch?v=CJT8wPnmjTM) - Overview of OTP supvervision trees
- [Fred Hebert - The Hitchhiker's Guide to the Unexpected](https://www.youtube.com/watch?v=W0BR_tWZChQ) - There's a good section on a real-world supervision trees

#### React/JS/Frontend

- [React Docs](https://reactjs.org/docs/getting-started.html) - much like Elixir, a great place to start
- [GraphQL Docs](https://graphql.org/learn/) - General GraphQL docs
- [Apollo GraphQL](https://www.apollographql.com/docs/react/) - Javascript library for GraphQL

### Example codebases
Some of these are convenient for reference

- [Evercam](https://github.com/evercam/evercam-server/) - Good example of supervision trees
- [EmCasa backend](https://github.com/emcasa/backend/) - Elixir backend for React app
- [EmCasa frontend](https://github.com/emcasa/frontend/) - React frontend for Elixir app
