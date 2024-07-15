# Marmalade - Musical Collaboration Over a Network in Subjective Real-Time

Music is math over time, and time is tricky - doubly so over a network. Marmalade forgoes the illusion of objective simultaneity, and embraces subjective simultaneity instead. Different observers will experience the events of the same musical session in different ways, and possibly in a different order, according to the flow of their local network, ending up with different and individual experiences. Just as they would if they were playing together in the same room.

With Marmalade, there is no central server acting as an objective source of truth. Instead, Marmalade offers a peer-to-peer protocol for composing and playing sounds in a way that is context aware. Each node in the network can publish these compositions, collect them, synchronize them, and play them in a meaningful and hopefully pleasing way.

## Core Concepts

A Marmalade jam session is made up of Marmalade audio generators which are sent and received by the nodes of the Marmalade network, referred to as players. These generators are pieces of software, composed by the participants in the jam, which each player runs locally. They query the state of the jam, which is subjective for each player, and determine which sounds to produce accordingly.

In addition, generators can manipulate designated parts of the state called tags, to allow other generators to synchronize with them when they decide what sounds to produce. The state of the jam on a specific player consists of the sate of the local hardware (clock, sound levels, audio setup, network connectivity), the state of the network the jam is being jammed on (number of participating players and their stats), and the tags set by all the generators that the player is playing.

For example, a player can start a jam by publishing a generator that plays a drum loop indefinitely, or for a specified duration, or one that plays the loop on every 10th second until the last player has left. This generator can also set and alternating "beat" tag in the state, allowing other generator to sync their audio to it. Another generator can count those "beats", and play a bass note on every 4th beat, or a melody on every 8th beat, adding a "verse" tag to the first and a "chorus" tag to the latter. Yet another generator can listen for the "verse" tag, and play a guitar riff on the second "beat" of every "verse" for ten minutes.

- `Jam`: a single instance of a musical collaboration session, consisting of a growing collection of generators composed by the participating players. Jams are identified by a name that is unique to all the players participating in the jam.
- `Player`: a single node participating in a jam, running the required software to handle composition, distribution and collection of generators, and the synchronized playback of the sounds they generate. Each player maintains its own version of the state of the jam. Players are identified by their public key, but for readability we use the first 8 characters of the Base64 encoded SHA-256 "fingerprint" of their public key as an identifier (with 63 represented as `_` instead of the more common `/`, so we can use it in file names). This limits our number of players to 64^8 at this time. Collisions, if they ever occur, will be considered invalid, so only the first player to use a public key with a given fingerprint will be accepted by each player. In the future we may use some kind of hash wordification or identicons to make the player IDs more readable.
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

Eventually, generators will probably be written in some a standard language, probably something we will come up with ourselves (see [producing audio](#producing-audio) below), hopefully something lispish.

For now, in order to postpone the decision of how to actually produce audio and because its fun, we keep the generators highly agnostic and implement them as directories packed into tgz files. To run a generator, the player unpacks the tgz file into a directory, enters it, and runs the first executable file it finds, providing it with the state of the jam and the ability to set tags and produce audio accordingly.

To keep things safe and standard, this should happen inside some kind of sandboxed virtual machine (see [Generator Isolation and Communication](#generator-isolation-and-communication) below).

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

The generator should be able to produce any kind of audio, synthesized or sampled - a single note, a bar, a riff, and silences of different durations. One way to acheive this is to give the generator instances access to the player's sound system, and let them write raw audio data to it. This is certainly the most versatile and efficient way to produce audio, but it's also harder to code and debug. Other options include:
- **[Mégra](https://megra-doc.readthedocs.io/en/latest/tutorial/organizing-sound/)** just looks awesome. Can use Jack or PipeWire, and holy shit, it looks awesome.
- **[Supercollider](https://supercollider.github.io/)** is very impressive, seems to be very widely supported and something of an industry standard. Seems to require more setup, and while able to work with PipeWire, requires Jack anyway.
- **[cl-patterns](https://github.com/defaultxr/cl-patterns)** is built on top of Supercollider and does it cooler and lispier. Seems a bit immature.
- **[Tidal Cycles](http://tidalcycles.org)** is also built on top of Supercollider but goes the Haskell way. Seems the most mature.
- **[Sonic Pi](https://sonic-pi.net/)** is also built on top of Supercollider, is very cool, and although it is built on Ruby it's still something we can learn from.

For all these options we will use Redis pub-sub to deliver the musical notation from generator to player. We can even use Redis's expiration mechanism to avoid playing sounds if the player hasn't gotten around to playing them in time.

### Generator Lifecycle

1. A generator is composed by a player.
2. The player requests all the players, self included, to `play` the generator, providing an arbitrary instance ID (current timestamp is a good choice).
3. Every player that receives the request checks that if the generator is locally known, `get`ting it from the requesting player if it is not.
4. Every player that receives the request deploys and runs the generator in its own microVM, with a socket file in `<jam name>/<player ID>/<generator name>/<instance ID>/`, unless that directory already exists (which means the generator was already `play`ed with this instance ID).
5. The generator instance reads the state of the jame and produces audio and sets tags accordingly.
6. The generator instance keeps running until it stops itself, or until the jam ends.

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
        auth
        <Redis persistency files>
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
        - `auth`: the Redis password for the jam.
        - `<Redis persistency files>`: the Redis aof and rdb files for the jam. This is the single source of truth for the local state of the jam.
- `players/` contains information about all the players the local player has ever known.
    - `<player ID>.lisp` contains the player's public key, address, and last seen time.

## Generator Isolation and Communication

Generators are currently just directories with arbitrary executables which the player sticks into a directory inside a [Firecracker microVM](https://firecracker-microvm.github.io/) running a lightweight linux distro, so that each generator gets a full virtual machine to can run stuff on, receiving a quota of CPU and memory, and a connection to Redis through which it can query the state, set tags, and produce audio.

TODO: We don't have quota's set on the Firecracker configuration.

The root filesystem of the Firecracker microVM is based on the [minimal Alpine linux docker image](https://hub.docker.com/_/alpine/), with a local init script that sets up the network and requests redis credentials on port `1234` and a tgz file on port `2468` from its host. Once the streams are received, the init script extracts the generator to a tmpfs, along with a file containing the credentials, and runs the first executable file in the directory. The generator runs as root on the virtual machine, and the only external access it has is to the Redis port on the host machine.

TODO: We should probably move to Firecracker snapshots of the machine to save some overhead, instead of booting a new machine every time.

Running the Firecracker microVM requires root access, and so does configuring the network. Moreover, all those interfaces and iptables rules to connect all the running generators to a local redis server are best kept in separate network namespaces, which also require root access. As does removing each namespace when it is no longer needed. In short, to run generators in a safe and isolated manner, root access is required.

Since running the player as root is not many people's cup of tea, Marmalade installs a little helper script that can be run as root by members of the `marmalade` group and performs one of three specific sets of tasks:
 1. If given the `--start-jam` flag, along with a jam name, create a new network namespace named `marmalade:<jam-name>` and run a Redis server inside it as the original calling user (presumably the one running the player) and using the jam's persistence directory as described above. If the namespace already exists it exits with an appropriate error message.
2. If given  the `--launch-generator` flag, along with a jam name, player ID, generator name, and instance ID, create a new network namespace named `marmalade:<jam-name>:<player ID>:<generator name>:<instance ID>`, connect it to the Redis namespace, run a Firecracker microVM inside it, and send it its redis credentials and the generator archive from the workdir described above. If the namespace already exists it exits with an appropriate error message.
3. If given the `--stop-jam` flag, along with a jam name, stop the Redis server running in the `marmalade:<jam-name>` namespace, delete the namespace, and then do the same thing with any remaining `marmalade:<jam-name>:*` namespaces and the Firefox instances running inside them.
For details, take a look at `install/marmalade-nslaunch`.

An interesting upshot of this is that if we want to move to another virtualization technology ([LXC](https://linuxcontainers.org/) is the only real compatitor right now), we can just modify the `marmalade-nslaunch` script to run something else under the same newtork namespace isolation and the player will keep running as before.

## Requirements

- This repository
- A linux machine with a sound card and a network connection (soundcard still not really required, or used)
- bash, curl, tar, gzip, and a handful of other basic GNU utilities
- [Redis](https://redis.io/) - currently we do not use the time series extension, but in the future we probably will, and that will require the time series module to be installed (or the entire Redis stack).
- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
    1. Download the `quicklisp.lisp` file from the Quicklisp website
    2. Run `sbcl --load <full path to quicklisp.lisp>`
    3. Optionally, set the quicklisp home directory with `(setf quicklisp-quickstart::*home* "<full path to quicklisp home>")`
    4. Run `(quicklisp-quickstart:install)`
    5. Run `(ql:add-to-init-file)`
    6. Create a symbolic link to this repository in the `local-projects` directory of the quicklisp home directory with `ln -s <full path to repository>/marmalade <full path to quicklisp home>/local-projects/marmalade`
- Compatible [Firecracker](https://firecracker-microvm.github.io/) setup
    1. Inside the `install` directory, run `make build` (which requires `make`, `bash`, `curl`, `jq`, `mkfs.ext4` and some basic GNU utilities; as well as root priviliges while building the rootfs image for chrooting - you will be prompted for your sudo password)
    2. Still inside the `install` directory run `sudo make install` to safely install the build artifacts on your system - this will automatically create the `marmalade` group and add the current user to it, and give the `marmalade` group the necessary permissions to run the `marmalade-nslaunch` script as root
    3. Optionally, run `sudo make marmalade-group <username1> <username2> ...` to add additional users to the `marmalade` group

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

To create a generator, you need to prepare a directory (anywhere on your filesystem) in which the first executable file is the entry point of your generator (see an example inside `workdir/sample_generator`, and then run `(marmalade:pack-generator "<path to generator directory>")` in the REPL. This will create a generator archive file in the `generators/` directory and thus register the generator, which you can verify by refreshing the `generators` endpoint. This will also provide you with the generator ID, composed of your player ID and the name of the generator directory you prepared. Once you have the generator ID, you can verify that its available for download by other players by visiting the `generator/<generator ID>` endpoint on your browser.

Before you run a generator, you need to start a jam by running `(marmalade:start-jam "<jam name>")` in the REPL. This will create a directory with the name of the jam inside the `jams/` directory, containing a `jam.log` file and a Redis persistence directory.

Now you should be able to ask the local player to run the generator using:
```lisp
(marmalade:request-play marmalade:*player-id* <generator name> <instance ID>)
```
The generator name should be the name of the generator directory you prepared, and the instance ID should be unique for each run of the generator. Note that the generator name is not the generator ID, which is the generator name prefixed by the player ID (in this case, the ID of the local player).

There are also `(marmalade:stop-jam "<jam name>")` and `(marmalade:stop-p2p-server)` functions that you can run in the REPL to stop the jam and the server, respectively, although the servers will also stop automatically if you exit the REPL.

## Plans

Currently, the player supports only commands to start and stop the jam, to create and run generators, and to communicate with other players. Other functionality that would be nice to have includes:
- Allowing and disallowing participants (currently the player will play whatever generators are requested of it, regardless of the source)
- Controlling the Master Volume, and the volume/max volume of individual tracks and generators (including muting and soloing)
- Controlling the tempo of the jam (which includes the ability to pause it)
- A GUI, probably web-based, instead of the REPL
- Visualizing the state of the jam, the generators and the tags; both in real-time and historically

In addition, there is a bunch of data that the local player should push into the state.

Oh, and our Firecracker setup still lacks quotas, and documenting the sample generator ain't a bad idea either.

## Thoughts

### Schedule

Maybe we create an evolving potential structure of a jam, represented as a directed graph of contexts (or tag collections). The schedule outlines the temporal structure of the jam without specifying the musical content. In many cases it makes sense to start jams with a common schedule for all players, outlining the basic structure of the jam, but as the schedule evolves over time it may end up being different for different players, and it may be interesting to start jams with different schedules for different players.

### Events

Maybe we want to allow generators to subscribe to events in the jam, such as the start of a new context, the end of a track, or the end of the jam. This could be handy for generators that need to know when to start or stop producing audio, or when to change the way they produce audio, without explicitly polling the state, and Redis has great pub-sub support.

Specifically, it seems that a some sort of "wait untill the state is X" operation can come in real handy, and easily be implemented with a pub-sub mechanism - with or without the local player's assistance.

### Permissioned Operations

Maybe some audio related operations should be restricted, or even completely disabled. For example, assuming we are using something like Tidal to produce audio, we will probably need to restrict the `hush` operation, which stops all audio, and `setcps`, which changes the tempo of the entire jam.

However, maybe we will want to allow certain generators - an obvious candidate being the generators of the local player that's actually running the jam - to perform such restricted operations.

In the same way, generators should generally be able to set tags only in their own namespace, but maybe we will want to allow certain generators to set tags in other namespaces, or even in the root namespace.
