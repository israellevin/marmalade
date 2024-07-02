# Marmalade - Musical Collaboration Over a Network in Subjective Real-Time

Music is math over time, and time is tricky - doubly so over a network. Marmalade forgoes the illusion of objective simultaneity, and embraces subjective simultaneity instead. Different observers will experience the events of the same musical session in different ways, and possibly in a different order, according to the flow of their local network, ending up with different and individual experiences. Just as they would if they were playing together in the same room.

With Marmalade, there is no central server acting as an objective source of truth. Instead, Marmalade offers a peer-to-peer protocol for composing and playing sounds in a way that is context aware. Each node in the network can publish these compositions, collect them, synchronize them, and play them in a meaningful and hopefully pleasing way.

## Core Concepts

A Marmalade jam session is made up of Marmalade audio generators which are sent and received by the nodes of the Marmalade network, referred to as players. These generators are pieces of software, composed by the participants in the jam, which each player runs locally. They query the state of the jam, which is subjective for each player, and determine which sounds to produce accordingly.

In addition, generators can manipulate designated parts of the state called tags, to allow other generators to synchronize with them when they decide what sounds to produce. The state of the jam on a specific player consists of the sate of the local hardware (clock, sound levels, audio setup, network connectivity), the state of the network the jam is being jammed on (number of participating players and their stats), and the tags set by all the generators that the player is playing.

For example, a player can start a jam by publishing a generator that plays a drum loop indefinitely, or for a specified duration, or one that plays the loop on every 10th second until the last player has left. This generator can also set and alternating "beat" tag in the state, allowing other generator to sync their audio to it. Another generator can count those "beats", and play a bass note on every 4th beat, or a melody on every 8th beat, adding a "verse" tag to the first and a "chorus" tag to the latter. Yet another generator can listen for the "verse" tag, and play a guitar riff on the second "beat" of every "verse" for ten minutes.

- `Jam`: a single instance of a musical collaboration session, consisting of a growing collection of generators composed by the participating players. Jams are identified by a name that is unique to all the players participating in the jam.
- `Player`: a single node participating in a jam, running the required software to handle composition, distribution and collection of generators, and the synchronized playback of the sounds they generate. Each player maintains its own version of the state of the jam. Players are identified by their public key, but for readability we use the first 8 characters of the Base64 encoded SHA-256 "fingerprint" of their public key as an identifier. This limits our number of players to 64^8 at this time. Collisions, if they ever occur, will be considered invalid, so only the first player to use a public key with a given fingerprint will be accepted by each player. In the future we may use some kind of hash wordification or identicons to make the player IDs more readable.
- `State`: the full history of the player, the network and the jam, which is subjective and local to each player. The state is used to determine which sounds to play, when and how.
D- `Generator`: the fundamental source of music in a jam - a generator is software that produces the right sound for the current state of the jam and sets appropriate tags that other generators can use in making their musical decisions. Generators are identified by a unique ID in the form `<player ID>:<generator name>`, where the name of the generator is unique to that player.
- `Tags`: attributes of the state which are set by individual generators.

## Network Protocol

This is the protocol for communication between players. It allows for the following requests to be made (and fulfilled) by any player to any other player in the network:
- `generators`: get a simple list of all generator IDs known to the queried player, in reverse chronological order.
- `generator <generator ID>`: get the generator with the specified ID as an archive file.
- `players`: get a list of all players known to the queried player, in reverse chronological order, containing, for each player, the public key identifying the player, their latest known addresses, and when they were last seen.
- `play <jam name> <generator name> <instance ID> <address> <public key> <signature>`: run the generator with the ID `<querying player ID>:<generator name>`, but only if it has not been run with the specified instance ID before (this allows for multiple instances of the same generator to be run in parallel). Because the player ID is generated from the public key, a player can only request for his own generators to be played. The address is the address of the querying player, the public key is the public key identifying him, and the signature is the signature on the query made by the corresponding private key.

While the first three calls do not change the state of the jam and can therefore be anonymous, the `play` call will, in addition to running the generator, download and deploy the generator if it is not locally present and add it to the list of known generators, and add the querying player to the list of players if it is not already there (along with the public key provided, which will be used to identify the player from that point on). It therefore has to be signed by the querying player and verified by the queried player.

### Cryptographic Details

#### Player Identification

Players, as stated above, are identified by their public keys. The software currently supports OpenSSH format private RSA keys, like those generated by `ssh-keygen -t RSA`, through the [cl-ssh-keys](https://github.com/dnaeon/cl-ssh-keys) library. The shorter player ID is simply a SHA-256 "fingerprint" of the public key truncated to 8 characters in Base64.

#### Signing Play Requests

1. Concatenate the request's parameters as a string in the form `<jam name>:<generator name>:<instance ID>:<address>`, where <jam name> is the name of the jam, <generator name> is the name of the generator (and not the generator ID, because a player can only request for his own generators to be played), <instance ID> is unique for every invocation of the generator (current timestamp is a very sensible choice), and <address> is the address at which the player can currently be reached.
2. Sign the sha256 digest of the string with the player's private key.
3. Encode the signature in Base64.

If you want to request a player to play a generator on your own, you can use curl and OpenSSL. To begin, however, you will need to use the `ssh-keygen` command to convert your keys from the OpenSSH format (which is what our software currently works with) to PEM format (which is used by OpenSSL):
```sh
cp ~/.ssh/id_rsa id_rsa.pem                       # Assuming your private key is in ~/.ssh/id_rsa
ssh-keygen -f id_rsa.pem -empem > id_rsa.pub.pem  # Get your public key
yes | ssh-keygen -f id_rsa.pem -pmpem             # To convert the private key you have to re-encrypt it in place
```

Now you can request the local player to play a generator on your behalf:
```bash
jam="jam"
generator="generator"
instance="instance"
address="address"
signature="$(echo -n "$jam:$generator:$instance:$address" | \
    openssl dgst -sha256 -sign id_rsa.pem -binary | openssl base64 -A)"
curl -XPOST "http://localhost:2468/play/$jam/$generator/$instance" --data "( \
    (:signature . \"$signature\") \
    (:pubkey . \"$(cat ~/.ssh/id_rsa.pub)\") \
    (:address . \"$address\"))"
```

## Generators and State

To postpone the decision of how to actually produce audio, and to keep generators as agnostic as possible, they are currently implemented simply as a directory with at least one executable file, which is packed into a tgz file and sent to the player that wants to run it. The generator can then read the state of the jam, set tags, and produce audio accordingly.

To keep things safe and standard, the Marmalade player sticks that directory inside a [Firecracker microVM](https://firecracker-microvm.github.io/) running a lightweight linux distro, and runs the first executable file in that directory.

Each generator gets a full virtual machine it can run stuff on, receiving a quota of CPU and memory, and a network connection through which it can query the state, set tags, and produce audio.

### Access to the State

The state of the jam is kept in a [Redis](https://redis.io/) instance that the player runs whenever it joins a jam (the instance is associated with the jam's Redis persistence directory). The Marmalade player has full read and write access to the state, and the generator instances have read access to the entire state, and write access to their own namespace in the state, in which they can set tags.

Redis is fast as hell, setting it up and granting generators access to the state should be a breeze, it has strong publish-subscribe capabilities in case we want to set up events, and it seems to have excellent time series support, which we can easily connect to a graphing tool like [Grafana](https://grafana.com/) and visualize the evolving state of the jam. That would be awesome.

#### Writing Tags

We give each generator instance write access to its own namespace in the state, in which tags can be written. Theoretically, the player can give a generator instance permissions to set tags in other areas of the state, which could create some sort of meta-generator operations, and cross-over operations, and all sorts of craziness if the need ever arises.

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

For all these options we will use Redis pub-sub to deliver the musical notation from generator to player. We can even use Redis's expiration mechanism to avoid playing sounds if the player hasn't gotten around to playing them in time.

### Generator Lifecycle

1. A generator is composed by a player.
2. The player requests all the players, self included, to `play` the generator, providing an arbitrary instance ID (current timestamp is a good choice).
3. Every player that receives the request checks that if the generator is locally known, `get`ting it from the requesting player if it is not.
4. Every player that receives the request deploys and runs the generator in its own microVM, with a socket file in `<jam name>/<player ID>/<generator name>/<instance ID>/`, unless that directory already exists (which means the generator was already `play`ed with this instance ID).
5. The generator instance reads the state of the jame and produces audio and sets tags accordingly.
6. The generator instance keeps running until it stops itself, or until the jam ends.

### Firecracker Isolation and Communication

Eventually, generators will probably be written in a standard language. Maybe Mégra, maybe [RestrictedPython](https://restrictedpython.readthedocs.io/en/latest/), maybe something we come up with ourselves. Until then, just for fun, we make the player run the generators, which are arbitrary executables, inside a Firecracker microVM.

Our current setup creates a base image (based on the minimal Alpine linux docker image) that firecracker can run, with a local init script that expects a tgz file on port `2468`. Once a tgz file is received, the init script extracts it to a tmpfs and runs the first executable file in the directory. The generator runs as root on the virtual machine, and the only external access it has is to the Redis port on the host machine, which it uses to read the state of the jam, write tags, and produce audio.

Running the Firecracker microVM requires root access, so the player uses a tiny executable with a `setuid` bit that creates a new tap device, configures it, modifies some firewall rules, and launches Firecracker as root. For added security and convenience, the binary only runs for users named "marmalade", and sets up all of the above in a separate network namespace also named "marmalade".

TODO: currently we have full network access, for development purposes, but it's just a matter of in `iptable` command. Also, this whole thing is really embryonic and under active development.

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
            firecracker.socket
          ...
        ...
      ...
      redis/
        <Redis files>
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
          - `firecracker.socket`: the socket controlling the generator's VM.
    - `redis/`: a Redis persistence directory for the jam.
      - `<Redis files>`: the Redis aof and rdb files for the jam. This is the single source of truth for the local state of the jam.
- `players/` contains information about all the players the local player has ever known.
  - `<player ID>.lisp` contains the player's public key, address, and last seen time.

## Requirements

- This repository
- A linux machine with a sound card and a network connection (soundcard still not really required, or used)
- A posix shell with curl tar and gzip
- [Redis](https://redis.io/) - currently we do not use the time series extension, but in the future we probably will, and that will require the time series module to be installed (or the entire Redis stack).
- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
  1. Download the `quicklisp.lisp` file from the Quicklisp website
  2. Run `sbcl --load <full path to quicklisp.lisp>`
  3. Optionally, set the quicklisp home directory with `(setf quicklisp-quickstart::*home* "<full path to quicklisp home>")`
  4. Run `(quicklisp-quickstart:install)`
  5. Run `(ql:add-to-init-file)`
  6. Create a symbolic link to this repository in the `local-projects` directory of the quicklisp home directory with `ln -s <full path to repository> <full path to quicklisp home>/local-projects/marmalade`
- [Firecracker](https://firecracker-microvm.github.io/) setup
  1. Run the `firecracker/prepare.sh` script (which requires `bash`, `curl`, `jq`, and some basic GNU utilities) - you should end up with the Firecracker binary, a kernel image and a root filesystem image which can receive and run a generator in a standard, secure and isolated environment
  2. Make and install the setuid wrapper for the tap device with a `make` command in the `firecracker` directory

## Running Marmalade

We've only just begun and there is no UI yet, so running the system is a bit of a pain in the ass.

Assuming you have a copy of the repository, start by going into the `workdir` directory and editing `config.lisp` to your liking (you can use any directory, as long as it contains a `config.lisp`). Then start a shell session within `workdir` and run SBCL with the Marmalade system loaded:
```sh
sbcl --eval '(ql:quickload "marmalade")'
```

Note: SBCL doesn't use readline by default, so you might want to install `rlwrap` or `rlfe` and prepend all `sbcl` calls with `rlfe -h ~/.sbcl_history`, like so:
```sh
rlfe -h ~/.sbcl_history sbcl --eval '(ql:quickload "marmalade")'
```

This will open a lisp REPL in which you can run commands, so start by running the p2p server with `(marmalade:start-p2p-server)`. The port on which the server runs is configurable in `config.lisp`, and in this documentation it's assumed to be `2468`. Once the server is running you can use your browser to access the following endpoints:
- http://localhost:2468/stats - web server stats
- http://localhost:2468/players - list of players (likely to only contain yourself)
- http://localhost:2468/generators - list of generators (likely to be `false`, indicating that you have not yet composed or collected any)
- http://localhost:2468/generator/<generator ID> - download a generator archive file (although you probably don't have any yet)

To create a generator, you need to prepare a directory (anywhere on your filesystem) in which the first executable file is the entry point of your generator, and then run `(marmalade:pack-generator "<path to generator directory>")` in the REPL. This will create a generator archive file in the `generators/` directory and thus register the generator, which you can verify by refreshing the `generators` endpoint. This will also provide you with the generator ID, composed of your player ID and the name of the generator directory you prepared. Once you have the generator ID, you can verify that its available for download by other players by visiting the `generator` endpoint on your browser.

Before you run a generator, you need to start a jam by running `(marmalade:jam-connect "<jam name>")` in the REPL. This will create a directory with the name of the jam inside the `jams/` directory, containing a `jam.log` file and a Redis persistence directory.

Now you should be able to ask the local player to run the generator using:
```lisp
(marmalade:request-play marmalade:*player-id* <generator name> <instance ID>)
```
The generator name should be the name of the generator directory you prepared, and the instance ID should be unique for each run of the generator. Note that the generator name is not the generator ID, which is the generator name prefixed by the player ID (in this case, the ID of the local player).

There are also `(marmalade:jam-disconnect "<jam name>")` and `(marmalade:stop-p2p-server)` functions that you can run in the REPL to stop the jam and the server, respectively, although the servers will also stop automatically if you exit the REPL.

## Thoughts

### Schedule

Maybe we create an evolving potential structure of a jam, represented as a directed graph of contexts (or tag collections). The schedule outlines the temporal structure of the jam without specifying the musical content. In many cases it makes sense to start jams with a common schedule for all players, outlining the basic structure of the jam, but as the schedule evolves over time it may end up being different for different players, and it may be interesting to start jams with different schedules for different players.

### Events

Maybe we want to allow generators to subscribe to events in the jam, such as the start of a new context, the end of a track, or the end of the jam. This could be handy for generators that need to know when to start or stop producing audio, or when to change the way they produce audio, without explicitly polling the state, and Redis has great pub-sub support.
