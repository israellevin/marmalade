# Marmalade - Musical Collaboration Over a Network in Subjective Real-Time

Music is math over time, and time is tricky - doubly so over a network. Marmalade forgoes the illusion of objective simultaneity, and embraces subjective simultaneity instead. Different observers will experience the events of the same musical session in different ways, and possibly in a different order, according to their local time and network, ending up with different and individual experiences. Just as they would if they were playing together in the same room.

With Marmalade, there is no central server acting as an objective source of truth. Instead, Marmalade offers a peer-to-peer protocol for composing and playing sounds in a way that is context aware. Each node in the network can publish these compositions, collect them, synchronize them, and play them in a meaningful and hopefully pleasing way.

## Core Concepts

A Marmalade jam session is made up of Marmalade audio generators which are sent and received by nodes on the Marmalade network, which are referred to as players. Each generator defines a function that can, while a player runs it, produce audio that matches the state of the jam on that player at that point. In addition, generators can register and set certain elements of that state, called tags. The state of the jam on a specific player consists of the sate of the local hardware (clock, sound levels, audio setup, network connectivity), the state of the network the jam is being jammed on (number of participating players and their stats), and the tags set by all the generators that the player is playing.

- `Jam`: a single instance of a musical collaboration session, consisting of a growing collection of generators composed by the participating players. Jams are identified by a name that is unique to all the players participating in the jam.
- `Player`: a single node participating in a jam, running the required software to handle composition, distribution and collection of generators, and the synchronized playback of the sounds they generate. Each player maintains its own version of the state of the jam. Players are identified by the first 8 characters of a SHA-256 hash of their public key.
- `State`: the full history of the player, the network and the jam, which is subjective and local to each player. The state is used to determine which sounds to play, when and how.
D- `Generator`: the fundamental source of music in a jam - a generator defines a function that produces the right audio for the current state of the jam and sets appropriate tags that other generators can use to make decisions. Generators are identified by a unique ID in the form `<player ID>:<generator name>`, where the name of the generator is unique to that player.
- `Tags`: attributes of the state which are set by individual generators.

For example, a player can start a jam by publishing a generator that plays a drum loop indefinitely, or for a specified duration, or one that plays the loop on every 10th second until the last player has left. This generator can also set and alternating "beat" tag in the state, allowing other generator to sync their audio to it.

## Network Protocol

This is the protocol for communication between players. It allows for the following requests to be made (and fulfilled) by any player to any other player in the network:
- `generators`: get a simple list of all generator IDs known to the queried player, in reverse chronological order.
- `generator <generator ID>`: get the generator with the specified ID as an archive file.
- `players`: get a list of all players known to the queried player, in reverse chronological order, containing, for each player, the public key identifying the player, their latest known addresses, and when they were last seen.
- `play <generator ID> <instance ID> <signature> <public key>`: run the generator with the specified ID, but only if it has not been run with the specified instance ID before (this allows for multiple instances of the same generator to be run in parallel).

While the first three calls do not change the state of the jam and can therefore be anonymous, the `play` call will, in addition to running the generator, download and deploy the generator if it is not locally present and add it to the list of known generators, and add the querying player to the list of players if it is not already there (along with the public key provided, which will be used to identify the player from that point on). It therefore has to be signed by the querying player and verified by the queried player.

## Player Directory Structure

```
<working directory>/
  config.lisp
  generators/
    <player ID:generator name>.tgz
    ...
  jams/
    <jam name>/
      jam.log
      <player ID>/
        <generator name>/
          <instance ID>/
            <generator files>
          ...
        ...
      ...
      redis/
        <redis files>
  players/
    <player ID>.lisp
    ...
```

- `config.lisp`: general configuration of the local player, such as the player's address and encryption keys.
- `generators/`: archive of all the generators that the local player has ever known.
  - `<player ID:generator name>.tgz`: a generator archive, containing the generator's files and directories.
- `jams/` the jams that the player participated in.
  - `<jam name>/`: working directory for a single jam.
    - `jam.log` debug log of the jam.
    - `<player ID>/`: all the generators played by a single player in the jam.
      - `<generator name>/`: all the instances of single generator played in the jam.
        - `<instance ID>/`: working directory for a single instance of the generator played in the jam.
          - `<generator files>`: the files of the generator instance.
    - `redis/`: a redis persistence directory for the jam.
      - `<redis files>`: the redis aof and rdb files for the jam. This is the single source of truth for the local state of the jam.
- `players/` contains information about all the players the local player has ever known.
  - `<player ID>.lisp` contains the player's public key, address, and last seen time.

## Generators

Generators are implemented as a directory in which the first executable file is the generator's entry point. A player wishing to play a generator will deploy it in a directory in the player's filesystem and execute the first executable in a separate process. This means that, at the moment, generators have to be in a format that is executable by the player's shell, and that they can completely fuck up the player's local environment. The first issue can be mitigated by running the generator with reduced privileges, chrooted to the generator's working directory, with CPU, RAM and FS quotas in place, and given access only to necessary resources (redis port). In the future we will probably run generators on a virtual machine, making our generators cross-platform.

The generator lifecycle is as follows:
1. A generator is composed by a player.
2. The player requests all the players, self included, to `play` the generator, providing an arbitrary instance ID (current timestamp is a good choice).
3. Every player that receives the request checks that if the generator is locally known, `get`ting it from the requesting player if it is not.
4. Every player that receives the request deploys and runs the generator in it's working directory, `<jam name>/<player ID>/<generator name>/<instance ID>/`, unless that directory already exists (which means the generator was already `play`ed with this instance ID).
5. The generator instance reads the state of the jame and produces audio and sets tags accordingly.
6. The generator instance keeps running until it stops itself, or until the jam ends.

### Access to the State

We can store the state in the player's RAM and come up with a simple API over a named pipe or a domain socket for generators to access the state of the jam, which will be very fast indeed. However, the state is essentially a time series, and we want the generators to query it in a way that is both fast and expressive, so we decided to save us some time and use an off-the-shelf time series database.

[Redis](https://redis.io/) seems to have excellent time series support, it's fast as hell, and setting it up and granting generators read access to the state should be a breeze. In addition, it's really easy to connect it to something like [Grafana](https://grafana.com/) which can visualize the evolving state of the jam, which could be awesome.

#### Writing Tags

We give each generator instance write access to its own namespace in the state, in which tags can be written. Theoretically, the player can give a generator instance permissions to set tags in other areas of the state, which could create some sort of meta-generator operations.

The ID of the player and the generator instance will be embedded in the path of the tags, for easy querying and to prevent collisions. In addition, we suggest the following optional tags for semantic clarity:
- `Composer`: a source of musical creation that composes sounds on a player. May be a person, a group, an algorithem, a set of wind-chimes or anything else that can operate a player. Multiple composers can compose sounds on a single player, and a composer can compose sounds on multiple players. This tag is useful for tracking the source of a sound, and for attributing sounds to their creators.
- `Track`: a musical stream within a jam, grouping together a set of sounds typically associated with a single player and a single composer and representing a single "instrument". This is useful for mixing and muting.
- `Context`: a named timeframe in the jam's schedule in which matching sounds are potentially played (e.g. 'buildup', 'drop', 'verse', 'chorus' or 'bar' - it makes sense to start jams with a basic rhythm generator defining some kind of an alternating 'beat'/'offbeat' context).

### Producing Audio

The generator function should be able to produce any kind of audio, synthesized or sampled - a single note, a bar, a riff, and silences of different durations. One way to acheive this is to give the generator instances access to the player's sound system, and let them write raw audio data to it. This is certainly the most versatile and efficient way to produce audio, but it's also harder to code and debug. Other options include:

- **[Mégra](https://megra-doc.readthedocs.io/en/latest/tutorial/organizing-sound/)** just looks awesome. Requires Jack or PipeWire, but holy shit, it looks awesome.
- **[Supercollider](https://supercollider.github.io/)** is very impressive, seems to be very widely supported and something of an industry standard. Seems to require more setup.
- **[cl-patterns](https://github.com/defaultxr/cl-patterns)** is built on top of Supercollider and does it cooler and lispier.
- **[Sonic Pi](https://sonic-pi.net/)** is something else we can learn from.

For all these options we can use redis pub-sub to deliver the musical notation from generator to player. We can probably use redis's expiration mechanism to avoid playing sounds if the player hasn't gotten around to playing them in time.

## Thoughts

### Schedule

Maybe we create an evolving potential structure of a jam, represented as a directed graph of contexts (or tag collections). The schedule outlines the temporal structure of the jam without specifying the musical content. In many cases it makes sense to start jams with a common schedule for all players, outlining the basic structure of the jam, but as the schedule evolves over time it may end up being different for different players, and it may be interesting to start jams with different schedules for different players.

### Events

Maybe we want to allow generators to subscribe to events in the jam, such as the start of a new context, the end of a track, or the end of the jam. This could be handy for generators that need to know when to start or stop producing audio, or when to change the way they produce audio, without explicitly polling the state.

## Requirements

- A linux machine with a sound card and a network connection
- A posix shell with curl tar and gzip
- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
  1. Download the `quicklisp.lisp` file from the Quicklisp website
  2. Run `sbcl --load <full path to quicklisp.lisp>`
  3. Optionally, set the quicklisp home directory with `(setf quicklisp-quickstart::*home* "<full path to quicklisp home>")`
  4. Run `(quicklisp-quickstart:install)`
  5. Run `(ql:add-to-init-file)`

## Running Marmalade

1. Clone this repository
2. Edit `config.lisp` to your liking
3. Create a `generators` directory in the player directory and fill it with generators to your liking
    1. Each generator should be a `tgz` file named `<player ID>:<generator name>.tgz` and containing a directory in which the first executable file is the generator's entry point
4. Run `sbcl --load src/run.lisp` (or `rlfe -h ~/.sbcl_history --load src/run.lisp` if you want readline support)
5. In the REPL run `(start-jam "<jam name>")`

Note: SBCL doesn't use readline by default, so you might want to install `rlwrap` or `rlfe` and prepend all `sbcl` calls with `rlfe -h ~/.sbcl_history`.
