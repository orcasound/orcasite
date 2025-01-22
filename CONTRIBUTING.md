# Contributing to Orcasound

Welcome to Orcasound! We're thrilled you're interested in contributing to the site.

- [Code of Conduct](#code-of-conduct)
- [Reporting Bugs](#reporting-bugs)
- [Requesting New Features](#requesting-new-features)
- [Contributing Code](#contributing-code)
- [Contacting Us](#contacting-us)
- [Resources](#resources)

## Code of Conduct

Please read and make sure you understand our [Code of Conduct](/CODE_OF_CONDUCT.md) before contributing to the project.

## Reporting Bugs

First, **ensure the bug was not already reported** by searching on GitHub under
[Issues](https://github.com/orcasound/orcasite/issues).

If you found a bug, you can help us by
[submitting a GitHub Issue](https://github.com/orcasound/orcasite/issues/new).
The best bug reports provide a detailed description of the issue and step-by-step instructions
for reliably reproducing the issue.

## Requesting New Features

You can request a new feature by [submitting a GitHub Issue](https://github.com/orcasound/orcasite/issues/new).

If you would like to implement a new feature, please first
[submit a GitHub Issue](https://github.com/orcasound/orcasite/issues/new) and
communicate your proposal so that the community can review and provide feedback. Getting
early feedback will help ensure your implementation work is accepted by the community.
This will also allow us to better coordinate our efforts and minimize duplicated effort.

## Contributing Code

Orcasite is currently covered by the [GNU Affero General Public License v3.0](https://github.com/orcasound/orcasite/blob/main/LICENSE). Permissions of this strongest copyleft license are conditioned on making available
complete source code of licensed works and modifications, which include larger works using a licensed
work, under the same license.  Copyright and license notices must be preserved.  Contributors provide
an express grant of patent rights.

If you are willing to follow the conditions of this license, check out the following links
to find an unassigned item to contribute to:

- ["Good first issue" GitHub issues for first-timers](https://github.com/orcasound/orcasite/labels/good%20first%20issue)
- [Project board](https://github.com/orgs/orcasound/projects/38/views/1)

Once you've chosen an issue and want to contribute code:

1. Fork the Orcasite repo.
2. Develop on a [feature branch](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow).
3. Submit a PR (don't review your own)!
    - To maintain a consistent style, we recommend running [Prettier](https://github.com/prettier/prettier) on js, and `mix format` before submission.
4. Once a PR is merged, we can throw it up onto the dev server to see how things look!

### Submitting a Pull Request

For all but the absolute simplest changes, first find an existing GitHub issue or
[submit a new issue](https://github.com/orcasound/orcasite/issues/new) so that the
community can review and provide feedback. Getting early feedback will help ensure your work
is accepted by the community. This will also allow us to better coordinate our efforts and
minimize duplicated effort.

If you would like to contribute, first identify the scale of what you would like to contribute.
If it is small (grammar/spelling or a bug fix) feel free to start working on a fix. If you are
submitting a feature or substantial code contribution, please discuss it with the maintainers and
ensure it follows the public roadmap. You might also read these two blogs posts on contributing
code: [Open Source Contribution Etiquette](http://tirania.org/blog/archive/2010/Dec-31.html) by Miguel de Icaza and
[Don't "Push" Your Pull Requests](https://www.igvita.com/2011/12/19/dont-push-your-pull-requests/) by Ilya Grigorik.
All code submissions will be reviewed and tested, and only those that meet
the bar for both quality and design/roadmap appropriateness will be merged into the source.

For all [pull requests](https://github.com/orcasound/orcasite/pulls) the following rules apply:
- The pull request description should describe the problem and solution, and reference the GitHub issue if one exists.
- Existing tests should continue to pass.
- Tests should ideally be included for every bug or feature that is completed.
- Documentation should be included for every feature that is end-user visible.
- Coding style should be consistent with the style used in other files of the same type in this repository.

## Contacting Us

### Questions?

We are happy to answer as best we can any question you have about Orcasite.
There are multiple ways you can ask questions:

- Using the [Zulip channel](https://orcasound.zulipchat.com/#narrow/stream/437031-orcasite)
- Using [Github discussions](https://github.com/orcasound/orcasite/discussions)

### Meetings

We meet for one hour every other Wednesday via Google Meet.  You can request a calendar invite or check the
[Zulip channel](https://orcasound.zulipchat.com/#narrow/stream/437031-orcasite) for the latest meeting
date and time information.

- [Request a calendar invite](https://forms.gle/Tegj4x6qxWx7PSes5)
- [Meeting link](https://meet.google.com/igp-gpdr-wwu)
- [Figma](https://www.figma.com/design/41JuvNkXJhTUob8HMsJiNC/Orcasite%3A-design-updates?node-id=882-3971)

### Maintainers

If you have any questions about parts of the architecture and site, these are contributors with context about various parts:

- [Paul Cretu](https://github.com/paulcretu) - Orcanode, streaming backend, ffmpeg
- [Skander Mzali](https://github.com/skanderm) - Elixir backend, JS/React frontend

## Resources

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
Some of these are convenient for reference:

- [Evercam](https://github.com/evercam/evercam-server/) - Good example of supervision trees
