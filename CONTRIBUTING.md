# Contributing to Orcasound

Welcome to Orcasound! We're thrilled you're interested in contributing to the site.

### General workflow
We recommend following these steps if you'd like to contribute to the repo.

0. Check out the [Trello board](https://trello.com/b/hRFh7Sc1/orcasite-development) for an overview of features being worked on.
    - This [public roadmap](https://trello.com/b/wBg0qhss/orcasound-roadmap) has a higher level view.
1. Fork the Orcasite repo
2. Develop on a [feature branch](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow)
3. Submit a PR (don't review your own)!
  	- To maintain a consistent style, we recommend running [Prettier](https://github.com/prettier/prettier) on js, and `mix format` before submission
4. Once a PR is merged, we can throw it up onto the dev server to see how things look!

### Questions?
If you have any questions about parts of the architecture and site, these are contributors with context about various parts:

- [Paul Cretu](https://github.com/paulcretu) - Orcanode, streaming backend, ffmpeg
- [Skander Mzali](https://github.com/skanderm) - Elixir backend, JS/React frontend

### Learning resources
If you're new to Elixir (or JS and React), that's no problem! Here are some resources for learning what they're about.

#### Elixir

- [Official Elixir Documentation](https://elixir-lang.org/getting-started/introduction.html) - An excellent place to get started.
- [Elixir Forum](https://elixirforum.com/) - A huge repository of discussion and answered questions
- [Elixir Slack](https://elixir-slackin.herokuapp.com/) - Super friendly bunch of people
- [Absinthe](https://hexdocs.pm/absinthe/overview.html) - Elixir library for GraphQL
- [Umbrella Projects](https://elixirforum.com/t/resources-on-how-to-build-and-structure-umbrella-projects-using-phoenix-1-3/11225) - Forum post with various resources for building a distributed architecture

##### Some cool videos

- [Micropatterns in Elixir](https://www.youtube.com/watch?v=9uvp4h7gXHg) - Great video on how to think about recursion to solve problems
- [An intro to OTP](https://www.youtube.com/watch?v=CJT8wPnmjTM) - Overview of OTP supvervision trees
- [Fred Hebert - The Hitchhiker's Guide to the Unexpected](https://www.youtube.com/watch?v=W0BR_tWZChQ) - There's a good section on a real-world supervision trees
- [ElixirConf - GraphQL with Absinthe & Dataloader](https://www.youtube.com/watch?v=m26i1L2D7Yk)
- [ElixirDaze - Phoenix Contexts](https://www.youtube.com/watch?v=l3VgbSgo71E) - Great video on how to think about Phoenix Contexts
- [Domain Driven Design: The Good Parts](https://www.youtube.com/watch?v=U6CeaA-Phqo) - Real-world contexts using DDD
- [DDD & Microservices](https://www.youtube.com/watch?v=yPvef9R3k-M) - Not Elixir specific, but an overview of how a distributed system can interact

#### React/JS/Frontend

- [React Docs](https://reactjs.org/docs/getting-started.html) - much like Elixir, a great place to start
- [GraphQL Docs](https://graphql.org/learn/) - General GraphQL docs
- [Apollo GraphQL](https://www.apollographql.com/docs/react/) - Javascript library for GraphQL

### Example codebases
Some of these are convenient for reference

- [Evercam](https://github.com/evercam/evercam-server/) - Good example of supervision trees
- [EmCasa backend](https://github.com/emcasa/backend/) - Elixir backend for React app
- [EmCasa frontend](https://github.com/emcasa/frontend/) - React frontend for Elixir app

## Code of Conduct

### Our Pledge

In the interest of fostering an open and welcoming environment, we as
contributors and maintainers pledge to making participation in our project and
our community a harassment-free experience for everyone, regardless of age, body
size, disability, ethnicity, gender identity and expression, level of experience,
nationality, personal appearance, race, religion, or sexual identity and
orientation.

### Our Standards

Examples of behavior that contributes to creating a positive environment
include:

* Using welcoming and inclusive language
* Being respectful of differing viewpoints and experiences
* Gracefully accepting constructive criticism
* Focusing on what is best for the community
* Showing empathy towards other community members

Examples of unacceptable behavior by participants include:

* The use of sexualized language or imagery and unwelcome sexual attention or
advances
* Trolling, insulting/derogatory comments, and personal or political attacks
* Public or private harassment
* Publishing others' private information, such as a physical or electronic
  address, without explicit permission
* Other conduct which could reasonably be considered inappropriate in a
  professional setting

### Our Responsibilities

Project maintainers are responsible for clarifying the standards of acceptable
behavior and are expected to take appropriate and fair corrective action in
response to any instances of unacceptable behavior.

Project maintainers have the right and responsibility to remove, edit, or
reject comments, commits, code, wiki edits, issues, and other contributions
that are not aligned to this Code of Conduct, or to ban temporarily or
permanently any contributor for other behaviors that they deem inappropriate,
threatening, offensive, or harmful.

### Scope

This Code of Conduct applies both within project spaces and in public spaces
when an individual is representing the project or its community. Examples of
representing a project or community include using an official project e-mail
address, posting via an official social media account, or acting as an appointed
representative at an online or offline event. Representation of a project may be
further defined and clarified by project maintainers.

### Enforcement

Instances of abusive, harassing, or otherwise unacceptable behavior may be
reported by contacting the project team at admin@orcasound.net. All
complaints will be reviewed and investigated and will result in a response that
is deemed necessary and appropriate to the circumstances. The project team is
obligated to maintain confidentiality with regard to the reporter of an incident.
Further details of specific enforcement policies may be posted separately.

Project maintainers who do not follow or enforce the Code of Conduct in good
faith may face temporary or permanent repercussions as determined by other
members of the project's leadership.

### Attribution

This Code of Conduct is adapted from the [Contributor Covenant][homepage], version 1.4,
available at [http://contributor-covenant.org/version/1/4][version]

[homepage]: http://contributor-covenant.org
[version]: http://contributor-covenant.org/version/1/4/
