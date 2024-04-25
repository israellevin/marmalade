# Jam - Musical Collaboration Over a Network in Subjective Real-Time

Music is math over time, and time is tricky - doubly so over a network. Jam forgoes the illusion of objective simultaneity, and embraces subjective simultaneity instead. Different participants will end up experiencing the same jam in different ways, just as they would if they were playing together in the same room.

## Core Concepts

- **Jam**: a single instance of a musical collaboration, consisting of the collective compositions and subjective experiences of at least one player.
- **Player**: a participant in a jam, corresponding to an instance of the Jam client, which is in charge of music composition, distribution, and playback.
- **Composer**: a source of musical creation that composes and publishes music through a player. May be a person, a group, an algorithem, a set of wind-chimes or anything else that can control a client.
- **Composition time**: the time at which a composer is composing a musical figure that may eventually get played. Composition time ends when the player receives the musical figure - this means that the composition time of the same figure ends at different times for different players.
- **Track**: a musical stream within a jam, produced by a single player (and often by a single composer and representing a single "instrument"). Players and composers can create and publish multiple tracks.
- **Figure**: the fundamental unit of music in a jam - associated with a specific track (and therefore a specific player) and intended to be played in a matching context and a matching state. A figure has three parts: a piece of code that evaluates to sound, a pattern matching the contexts it can play in, and a piece of code that matches the state in which the figure should be played (according to the composer, anyway).
- **Context**: a named timeframe in the jam's schedule in which matching figures are potentially played (e.g. 'buildup', 'drop', 'verse', 'chorus', and even 'bar' and 'beat'). Contexts are defined by a function that takes the current state of the jam and returns a boolean indicating whether the context is active, and are connected by a directed (and potentially cyclic) graph that defines the schedule of the jam. A context can contain a list of nested contexts, which are evaluated in order to determine which of them are active.
- **State**: the current state of the jam, of which the context is (probably the most imprtant) part. Figures are matched to contexts based on their name, but their playback may be further conditioned upon the local state of the jam. Likewise, contexts start and end based on the state of the jam, and not just the schedule. Note that states are entirely subjective and differ between players across time and space.
- **Schedule**: the evolving potential structure of a jam, represented as a directed graph of contexts. The schedule outlines the temporal structure of the jam without specifying the musical content. In many cases it makes sense to start jams with a common schedule for all players, outlining the basic structure of the jam, but as the schedule evolves over time it may end up being different for different players, and it may be interesting to start jams with different schedules for different players.
- **Arrangement**: the potential sequencing of musical figures in the jam, built on top of the schedule. The arrangement, which grows as figures are composed, published, and matched to contexts, is effected by the time in which figures are received by the player and the local state of the jam, so it is potentially different for each player.
- **Arrangement time**: the time at which a player matches a figure to a context, adding it to the local arrangement.
- **Playback time**: the time in which a player plays the figures scheduled by the arrangement. It is in this time that the state of the jam is evaluated to determine which figures on the arrangement to play and how to play them.

A jam has of a number of players, each operated by a number of composers, producing a number of tracks, each made up of a list of musical figures. During a jam, players compose and publish figures, which are matched to contexts independently by each player, becoming a part of their local arrangement. Each player, independently, starts and ends contexts according to the local state and the schedule, and plays the figures indicated by the arrangement.

## Flow

1. A player starts a jam by composing and publishing a schedule of contexts - the initial skeleton on which future arrangements will be fleshed out - plus some initial figures.

```lisp
'(
  (start . buildup)
  (start . intro)
  (intro . verse)
  (verse . chorus)
  (chorus . drop)
  (chorus . verse)
  (drop . buildup)
)
```

2. An additional player joins the jam, and starts composing figures. The new player's figures are published and added to both arrangements.

## Changing the Schedule
