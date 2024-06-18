# Marmalade - Musical Collaboration Over a Network in Subjective Real-Time

Music is math over time, and time is tricky - doubly so over a network. Marmalade forgoes the illusion of objective simultaneity, and embraces subjective simultaneity instead. Different observers will experience the events of the same musical session in different ways, and possibly in a different order, according to their local time and network, ending up with different and individual experiences. Just as they would if they were playing together in the same room.

With Marmalade, there is no central server acting as an objective source of truth. Instead, Marmalade offers a peer-to-peer protocol for composing and playing sounds in a way that is context aware. Each node in the network can publish these compositions, collect them, synchronize them, and play them in a meaningful and hopefully pleasing way.

## Core Concepts

A Marmalade jam session is made up of Marmalade audio generators which are sent and received by nodes on the Marmalade network, which are referred to as players. Each generator defines a function that can, while a player runs it, produce audio that matches the state of the jam on that player at that point. In addition, generators can register and set certain elements of that state, called tags. The state of the jam on a specific player consists of the sate of the local hardware (clock, sound levels, audio setup, network connectivity), the state of the network the jam is being jammed on (number of participating players and their stats), and the tags set by all the generators that the player is playing.

- `Jam`: a single instance of a musical collaboration session, consisting of a growing collection of generators composed by the participating players. Jams are identified by a name that is unique to all the players participating in the jam.
- `Player`: a single node participating in a jam, running the required software to handle composition, distribution and collection of generators, and the synchronized playback of the sounds they generate. Each player maintains its own version of the state of the jam. Players are identified by their public key, but for readability we use the first 8 characters of the Base64 encoded SHA-256 "fingerprint" of their public key as an identifier. This limits our number of players to 64^8 at this time. Collisions, if they ever occur, will be considered invalid, so only the first player to use a public key with a given fingerprint will be accepted by each player. In the future we may use some kind of hash wordification or identicons to make the player IDs more readable.
- `State`: the full history of the player, the network and the jam, which is subjective and local to each player. The state is used to determine which sounds to play, when and how.
D- `Generator`: the fundamental source of music in a jam - a generator defines a function that produces the right audio for the current state of the jam and sets appropriate tags that other generators can use to make decisions. Generators are identified by a unique ID in the form `<player ID>:<generator name>`, where the name of the generator is unique to that player.
- `Tags`: attributes of the state which are set by individual generators.

For example, a player can start a jam by publishing a generator that plays a drum loop indefinitely, or for a specified duration, or one that plays the loop on every 10th second until the last player has left. This generator can also set and alternating "beat" tag in the state, allowing other generator to sync their audio to it.

## Network Protocol

This is the protocol for communication between players. It allows for the following requests to be made (and fulfilled) by any player to any other player in the network:
- `generators`: get a simple list of all generator IDs known to the queried player, in reverse chronological order.
- `generator <generator ID>`: get the generator with the specified ID as an archive file.
- `players`: get a list of all players known to the queried player, in reverse chronological order, containing, for each player, the public key identifying the player, their latest known addresses, and when they were last seen.
- `play <jam name> <generator name> <instance ID> <address> <public key> <signature>`: run the generator with the ID `<querying player ID>:<generator name>`, but only if it has not been run with the specified instance ID before (this allows for multiple instances of the same generator to be run in parallel). Because the player ID is generated from the public key, a player can only request for his own generators to be played. The address is the address of the querying player, the public key is the public key identifying him, and the signature is the signature on the query made by the corresponding private key.

While the first three calls do not change the state of the jam and can therefore be anonymous, the `play` call will, in addition to running the generator, download and deploy the generator if it is not locally present and add it to the list of known generators, and add the querying player to the list of players if it is not already there (along with the public key provided, which will be used to identify the player from that point on). It therefore has to be signed by the querying player and verified by the queried player.

### Cryptographic Details

#### Player Identification

Players, as stated above, are identified by their public keys. The software currently supports OpenSSH format private keys, like those generated by `ssh-keygen`, through the [cl-ssh-keys](https://github.com/dnaeon/cl-ssh-keys) library, which supports many key types (although only RSA keys have been tested). The shorter player ID is simply a SHA-256 hash of the public key truncated to 8 characters.

#### Signing Play Requests

1. Concatenate the request's parameters as a string in the form `<jam name>:<generator name>:<instance ID>:<address>`, where <jam name> is the name of the jam, <generator name> is the name of the generator (and not the generator ID, because a player can only request for his own generators to be played), <instance ID> is unique for every invocation of the generator (current timestamp is a very sensible choice), and <address> is the address at which the player can currently be reached.
2. Hash the string, treated as a byte array, with SHA-256 to produce a digest.
3. Sign the digest with the player's private key.
4. Encode the signature in Base64.

##### Technical Annoyance

Our current implementation uses [Ironclad](https://github.com/sharplispers/ironclad) for signature generation and verification (the whole thing is in the rather short `src/crypto.lisp` file, and in the Ironclad source code in `src/public-key/rsa.lisp`). Sadly, when I try to do the exact same thing with OpenSSL, I get a different signature, which fails the lisp verification (and vice versa).

I'm considering trying something other than Ironclad, but there aren't many options. Maybe running OpenSSL from lisp as a subprocess. Maybe drop lisp and write the infrastructure in something a little more... Sane? In any event, just in case someone else (future me?) ever tries to figure this out, here are my investigations, showing that the problem probably lies at some implicit padding stage in the RSA signature generation process.

To begin, we need to convert our OpenSSH keys (the format created by `ssh-keygen`, which is what our software currently works with) to PEM format, which is used by OpenSSL (don't even think of using `ssh-keygen -f ~/.ssh/id_rsa -Y sign`, it requires a namespace to be added and is very uncontrolable):
```sh
cp ~/.ssh/id_rsa id_rsa.pem                       # Assuming your private key is in ~/.ssh/id_rsa
ssh-keygen -f id_rsa.pem -empem > id_rsa.pub.pem  # Get your public key
yes | ssh-keygen -f id_rsa.pem -pmpem             # To convert the private key you have to re-encrypt it in place
ssh-keygen -f ./id_rsa.pem -lEsha256              # To get the public key fingerprint
```

You can compare the fingerprints of the old key and the new key to make sure you didn't mess up the conversion:
```sh
fingerprint() { ssh-keygen -lEsha256 -f "$1" | grep -Po '(?<=SHA256:)[^ ]*'; }
diff <(fingerprint ~/.ssh/id_rsa) <(fingerprint ./id_rsa.pem)
```

Now we can create a OpenSSL based shell implementation for signing and verifying a message:
```bash
sign_play_request() {
    local jam="$1"
    local generator="$2"
    local instance="$3"
    local address="$4"
    echo -n "$jam:$generator:$instance:$address" | \
        openssl dgst -sha256 -sign id_rsa.pem -binary | openssl base64
}

verify_play_request() {
    local jam="$1"
    local generator="$2"
    local instance="$3"
    local address="$4"
    local signature="$5"
    echo -n "$jam:$generator:$instance:$address" | \
        openssl dgst -sha256 -verify id_rsa.pub.pem -signature <(openssl base64 -d <<< "$signature")
}

verify_play_request jam generator instance address "$(sign_play_request jam generator instance address)"
verify_play_request jam generator instance address "$(sign_play_request jam wrong-generator instance address)"
```

The following, however, does not work:
```sh
curl -XPOST http://localhost:2468/play/jam/generator/instance --data "( \
    (:signature . \"$(sign_play_request jam generator instance address)\") \
    (:pubkey . \"$(cat ~/.ssh/id_rsa)\")
    (:address . \"address\"))"
```

We can verify that the digest process produces binary identical output in both lisp and OpenSSL:
```lisp
* (marmalade:play-request-digest "jam" "generator" "instance" "address")
#(142 236 104 35 78 192 215 30 54 26 146 120 37 0 252 118 131 163 214 178 58
  116 97 24 111 136 189 91 232 59 98 50)
```
```sh
echo -n "jam:generator:instance:address" | openssl dgst -sha256 -binary | od -An -vtu1
 142 236 104  35  78 192 215  30  54  26 146 120  37   0 252 118
 131 163 214 178  58 116  97  24 111 136 189  91 232  59  98  50
```

That both implementations produce the same integer representation of the digest:
```lisp
* (ironclad:octets-to-integer (marmalade:play-request-digest "jam" "generator" "instance" "address"))
64646119139623414069918042435346499684139792742605948436315543147541968937522
```
```sh
echo "ibase=16; $(echo -n "jam:generator:instance:address" | openssl dgst -sha256 -binary | xxd -p | tr -d "\n" | tr 'a-f' 'A-F')" | bc
64646119139623414069918042435346499684139792742605948436315543147541\
968937522
```

And that the public key is read identically by both cl-ssh-keys and OpenSSL:
```lisp
(ssh-keys:rsa-key-modulus *player-key*)
(ssh-keys:rsa-key-exponent *player-key*)
(ssh-keys:rsa-key-prime-p *player-key*)
(ssh-keys:rsa-key-prime-q *player-key*)
```
```bash
print_chunk() {
    [ "$component" ] || return
    echo "ibase=16;$(tr -d " :" <<< "$hex")" | BC_LINE_LENGTH=0 bc
    unset component hex
}
IFS=''
while read line; do
    if grep -q "^[[:space:]]" <<< "$line"; then
        [ "$component" ] && hex="$hex${line^^}"
    else
        print_chunk
        grep -q "^\(modulus\|privateExponent\|prime[12]\):$" <<< "$line" && echo "---$line" && component="$line"
    fi
done < <(openssl rsa -in ./key.pem -text -noout)  # This should be the PEM file you created earlier.
print_chunk
```

The problem can be in the exponentiation, which is highly unlikely, or with the padding, which shouldn't be happning at all but obviously still does. The next thing to do is probably to try all the options for `(ironclad:sign-message :pss)` and all the options for OpenSSL (probably using `pkeyutl` with `-sign -pkeyopt rsa_padding_mode:???` and `-rawin` and its ilk), and see if anything rhymes.

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

## Requirements

- A linux machine with a sound card and a network connection
- A posix shell with curl tar and gzip
- [Redis](https://redis.io/) - currently we do not use the time series extension, but in the future we probably will, and that will require the time series module to be installed (or the entire redis stack).
- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
  1. Download the `quicklisp.lisp` file from the Quicklisp website
  2. Run `sbcl --load <full path to quicklisp.lisp>`
  3. Optionally, set the quicklisp home directory with `(setf quicklisp-quickstart::*home* "<full path to quicklisp home>")`
  4. Run `(quicklisp-quickstart:install)`
  5. Run `(ql:add-to-init-file)`
  6. Create a symbolic link to this repository in the `local-projects` directory of the quicklisp home directory with `ln -s <full path to repository> <full path to quicklisp home>/local-projects/marmalade`

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

Before you run a generator, you need to start a jam by running `(marmalade:jam-connect "<jam name>")` in the REPL. This will create a directory with the name of the jam inside the `jams/` directory, containing a `jam.log` file and a redis persistence directory.

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

Maybe we want to allow generators to subscribe to events in the jam, such as the start of a new context, the end of a track, or the end of the jam. This could be handy for generators that need to know when to start or stop producing audio, or when to change the way they produce audio, without explicitly polling the state, and redis has great pub-sub support.
