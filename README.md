Real-time elevator system for TTK4145
=====
[![forthebadge](https://forthebadge.com/images/badges/fuck-it-ship-it.svg)](https://forthebadge.com)
[![Build Status](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels.svg?token=VaZNdDabsDWKmoAAY6fP&branch=master)](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels)


![elevator stairs](https://media.giphy.com/media/CYEadxFZFtcNG/giphy.gif)


Release check list:

- [ ] EPMD må være startet, eller startes
- [ ] egen start.sh kan lages for å forsikre epmd og starte bin scriptet. Den kan forsovet også starte elevator-server med riktig port!
- [ ] må være utenfor bin-mappen for å kjøre start scriptet ´./bin/dev_elevator-0.0.1´
- [ ] Hall_request_assigneren må i apps i release mappen.

- [ ] IP for broadcast, og IP for elevator-server må være på stell.
- [ ] A release on linux will run on linux (no cross-platform releases)
- `include_erts` gjør at erlang ikke trenger å være installert, og sikrer riktig kjøring uansett versjon på maskinen

Architecture
----
![Elevator diagram](https://github.com/TTK4145/project-wrong_on_so_many_levels/raw/master/doc/Sanntid_moduler.png)

----

# Rebar3
## Run
    $ rebar3 shell --sname one --apps elevator
## Compile
    $ rebar3 compile
## Release
    $ rebar3 release

# Built using
- `Erlang`
- [jsone](https://github.com/sile/jsone)
- [Elevator driver](https://github.com/TTK4145/driver-erlang)
- [Hall request assigner](https://github.com/TTK4145/Project-resources/tree/master/cost_fns/hall_request_assigner)
----

![elevator spam](https://media.giphy.com/media/MNepUUKWUjvi0/giphy.gif)