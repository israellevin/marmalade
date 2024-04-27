# Marmalade - Musical Collaboration Over a Network in Subjective Real-Time

Music is math over time, and time is tricky - doubly so over a network. Marmalade forgoes the illusion of objective simultaneity, and embraces subjective simultaneity instead. Different observers will experience the events of the same musical session in different ways, and possibly in a different order, according to their local time and network, ending up with different experiences of the same musical event. Just as they would if they were playing together in the same room.

With Marmalade, there is no central server providing a single source of truth. Instead, Marmalade offers a peer-to-peer protocol for composing sounds in a way that is context aware. Each node in the network can publish these sounds, collect them, synchronize them, and play them in a meaningful and hopefully pleasing way.

## Core Concepts

A Marmalade jam session is made up of Marmalade events which are sent and received by Marmalade nodes referred to as players. Each event defines a function that can, once a player runs it, produce audio that matches the state of the jam on that player at that point, as well as register and set certain elements of that state called tags. The state of the jam consists of the sate of the local hardware (clock, sound levels, audio setup, network connectivity), the state of the Marmalade network the jam is being jammed on (number of peers and their stats), and the state of the events that the player plays and their tags.

- **Jam**: a single instance of a musical collaboration session, consisting of a growing collection of events composed by the participating players.
- **Player**: a single node in a jam, running the required software to handle composition, distribution, and synchronized playback of sounds. Each player maintains its own version of the state of the jam.
- **State**: the full history of the player, the network and the jam, which is subjective and local to each player. The state includes the event tags which are active from the player's perspective, and is used to determine which sounds to play, when and how.
- **Event**: the fundamental unit of music in a jam - an event defines a function that produces the right audio for the current state of the jam and sets the right tags.
- **Tags**: attributes of the state which are set by individual events.

For example, a player can start a jam by publishing an event that plays a drum loop indefinitely, or for a specified time, but he can also start with an event that plays the loop on every 10th second until the last player has left. This event can also set and alternating "beat" tag in the state, allowing other events to be played only on of off it.

### Tags

Each event has a unique name and is provisioned with a namespace in which it can set tags. This namespace contains an automatic and immutable tag identifying the player that composed it. In addition, we suggest the following tags for semantic clarity:

- **Composer**: a source of musical creation that composes sounds on a player. May be a person, a group, an algorithem, a set of wind-chimes or anything else that can operate a player.
- **Track**: a musical stream within a jam, grouping together a set of sounds. Each player can produce multiple tracks, but a track is always produced by a single player, and typically by a single composer, representing a single "instrument".
- **Context**: a named timeframe in the jam's schedule in which matching events are potentially played (e.g. 'buildup', 'drop', 'verse', 'chorus' or 'bar' - it makes sense to start jams with a basic rhythm event defining some kind of an alternating 'beat'/'offbeat' context).

### Audio

The event function can produce any kind of audio - a single note, a bar, a riff, silences of different durations, synthesized or sampled. For starters, we will just use LISP code that gets evaluated, we just need to decide which sound library we use. Later we should either run this in a per-event sandbox, or define our own language for audio production (and for state reading and tag setting).

During the jam each node becomes aware of a growing collection of events whose functions it can run.

### Schedule

Maybe we create an evolving potential structure of a jam, represented as a directed graph of contexts. The schedule outlines the temporal structure of the jam without specifying the musical content. In many cases it makes sense to start jams with a common schedule for all players, outlining the basic structure of the jam, but as the schedule evolves over time it may end up being different for different players, and it may be interesting to start jams with different schedules for different players.
