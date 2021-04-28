The Dune workflow
=================

Notes:
- clarify when should someone feel confident everyone has read a PR
- clarify that someone opening a PR should have read it

The Dune workflow aims to provide the best user experience in a way
that is manageable for the Dune team. It is centered around three main
components: github, live text discussion via slack and regular
video-conf meetings. This document explains how we make use of these
to build a smooth workflow.

## Github issues and pull requests

As Dune is a big open source projects, a lot of issues and pull
requests are opened regularly. This generates a large amount of
notifications. As such, it is not realistic for each developer to
watch the bulk of github notifications.

Still, all new issues and pull requests need to be triaged, and we
must make sure that reporters get feedback on their requests in a
timely manner.

To make this work, we make use of two mechanisms:

- a slack channel to receive notifications in real time about newly
  opened issues and pull requests
- regular meetings to go over issues and pull requests that needs
  triaging

The meetings are intended as a "catch-all" to make sure no issue or
pull request goes unanswered. The slack channel is intended to speed
up the triaging of straighforward issues and pull requests. It is
expected that during the meetings, we only discuss issues and pull
requests that either no one noticed or require a team discussion.

The slack channel is the public channel `#dune-github` on
https://ocamllabs.slack.com/. The rationale for relying on a channel
rather than emails is that there is no need for everyone to go over
each new notification. If someone happens to see something they can
help with, that's great. Otherwise it will be dealt with at the next
meeting.

## Triaging issues and pull requests

We want to ensure that each newly opened issue and pull request ends
up in one of the following state in a fixed period of time:

- closed or merged
- assigned to someone
- postponed

## One person assigned to each issue or PR

For each github issue or PR, there must be one person who is
responsible for it. It is this person responsibility to ensure that:

- the issue/PR eventually gets closed or merged
- the issue/PR never stalls

If the conclusion of a discussion is that some work needs to be done,
it is not necessarily the responsibility of this person to do this
work. Though if the work is a small bugfix that would benefit Dune
users and no one has volunteered to write the fix, it is expected that
the assignee will write the fix and submit a pull request.

For bug reports, it is the responsibility of this person to make sure
the bug is reproducible with a minimal example. It is not their
responsibility to come up with the minimal example, however it is
their responsibility to lead the discussion to obtain such a minimal
example. For instance, by asking questions.

While the assignee is responsible for the issue, other developers are
still free to participate in the discussion and help it move
forward. In fact, the assignee doesn’t necessarily have to be the
person talking the most and they can even not participate. However, if
the discussion stalls they are responsible for making it move forward.

If the assignee doesn’t have the required technical knowledge to deal
with a particular issue or PR, they should ask on the `#dune-dev`
slack channel.

We will use the `Assignees` field on github to indicate who is
assigned to the issue or PR.

## Merging pull requests

We want to ensure that the tip of Dune is in a good state at all
time. For instance, that tests are passing and that the code is in
line with the goal of Dune.

When merging a pull request, we should ensure that:
- the new tip is still in a good state
- the new tip is somewhat better than the old one

Anyone who is confident that merging a PR will respect these two
points should feel free to go ahead and merge it.

To increase our confidence that these two points are respected, we ask
that each PR is reviewed by at least two people. One of the reviewer
can be the author of the PR, although a large PR written by a non-Dune
developer should probably be reviewed by at least two Dune developers.

When someone has fully reviewed a PR, they should signal it by
clicking the "Approve" button.

Note: for multi-PRs features, such as large refactorings, it is OK to
merge PRs that don't add new feature, don't fix bugs and leave the
code in a slightly worst state, as long as the remaining PRs follow
shortly after.

## When it is not clear how to proceed

Sometimes, the discussion reaches a point where it is not clear what
to do. For instance it can be because controversial features are being
discussed, or because the scope of the discussion becomes wider than
the original subject. In any case, while some issues or PRs can be
dealt with by a single person, some other require wider team
discussions.

When the discussion reaches such a point, the assignee should mark it
as such. We will do that by adding the label
`requires-team-discussion`.

## When the discussion is premature

Sometimes, while a feature request or pull request is valid on its
own, it doesn’t play nicely with other planned or on-going work and it
is better to wait before dealing with it. It can also be that we
simply don’t know to move forward at this point.

In such cases, we should decide whether:

- the feature/bugfix discussed is something we think would improve
  Dune in general
- the feature/bugfix discussed is more marginal or unclear

In the first case, we should add the label `postponed` to the
issue. In the latter, we should close it.

## Regular team meetings

During our regular team meetings, we will look at the list of issues
and PRs:

- that are unassigned
- that have the label `requires-team-discussion`

For each of them, we will have a short discussion and decide what to
do. For each issue or PR, the outcome can be one of:

- we assign it to someone
- we close it
- we add the label `postponed`

In between meetings, if someone sees an issue that is un-assigned and
they know how to deal with it and have the bandwidth to do it, they
should still feel free to assign it to themselves immediately.

The main goal of the meetings will be to clear the list issues that
are unassigned or have the label
`requires-team-discussion`. Initially, we will continue with one
meeting every two weeks. However, if the rate of such issues and PRs
increases and we can’t go through the stack in one meeting, we will
increase the frequency of the meetings to one every week.  Reviewing
postponed issues and PRs

Twice a year, we will review the list of issues and PRs with the label
`postponed` and decide whether:

- to assign it to someone or change the assignment
- to close it
- to keep the `postponed` label

Such bi-annunal reviews will happen druing the first meeting of March
and the first meeting of September.

### Private meeting notes

During the meeting, we will take notes in a shared google doc. At each
meeting, one person will be responsible for taking the notes. This
person will be either a volunteer or will be chosen at random.

This document is private and aimed at the Dune team only.

### Public minutes

After each meeting, the person who took the notes will publish minutes
of the meeting on the [Dune wiki][wiki]. The aim is to provide a
summary of what was dicussed and decided during the meeting for future
reference, and to allow the community to follow the development of
Dune. The minutes will include the list of attendees.

## Assigning issues and PRs fairly

When there is no clear person who should deal with an issue or PR, we
will pick one person at random from the team. The Dune team is
composed of various people from different organizations who do not
have the same time budget to work on Dune. We will keep a list of the
team members and for each of them indicate how many days a week they
feel they can spend working on Dune issues and PRs, excluding work
that goes towards their own interest or their companies interest. We
will keep this list in the [Dune wiki][wiki].

When assigning issues or PRs, the random selector will try to keep the
number of issues/PRs assigned to each person proportional to the
amount of time they can spend on Dune each week.

When new members join in and after an integration period, we can
rebalance the existing assignments.

## Opening draft PRs

Opening PRs for a work in progress is a good thing as other people can
see what we are working on and collaborate. It can also be a good way
to ask for feedback. However, good review is hard work and we should
be mindful of the reviewing time of others. As such, when opening a PR
that is not ready it should be clearly marked as such. This can be
dune by adding `[WIP]` in the title or ticking the "Draft" box when
creating the PR.

Once the PR is ready, the `[WIP]` prefix or "Draft" status should be
removed so that others know it is ready for review.

We don't have a protocol to determine who should review a PR at the
moment, so for now we are relying on shared goals between people and
good will. Asking on #dune-dev is another way to find reviewers when
needed.

[wiki]: https://github.com/ocaml/dune/wiki
