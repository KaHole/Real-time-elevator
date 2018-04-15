elevator project (電梯)
=====
[![forthebadge](https://forthebadge.com/images/badges/fuck-it-ship-it.svg)](https://forthebadge.com)

Real-time elevator system for TTK4145

Modules
----

- [ ] Gå over kriterier for evaluering og spec for å få god karakter
- [ ] `erl_tidy` kan brukes for formattering og optimalisering
- [ ] Style guide: https://github.com/inaka/erlang_guidelines

Release
----
    $ rebar3 release

Release check list:

- [ ] EPMD må være startet, eller startes
- [ ] egen start.sh kan lages for å forsikre epmd og starte bin scriptet. Den kan forsovet også starte elevator-server med riktig port!
- [ ] må være utenfor bin-mappen for å kjøre start scriptet ´./bin/dev_elevator-0.0.1´
- [ ] Hall_request_assigneren må i apps i release mappen.

- [ ] IP for broadcast, og IP for elevator-server må være på stell.
- [ ] A release on linux will run on linux (no cross-platform releases)
- `include_erts` gjør at erlang ikke trenger å være installert, og sikrer riktig kjøring uansett versjon på maskinen

----

Build & Run
----
    $ rebar3 shell --sname one --apps elevator

Build
----
    $ rebar3 compile


![Elevator diagram](https://github.com/TTK4145/project-wrong_on_so_many_levels/raw/master/doc/Sanntid_moduler.png)

----

Built using
----
- `Erlang`
- [jsone](https://github.com/sile/jsone)
- [Elevator driver](https://github.com/TTK4145/driver-erlang)
- [Hall request assigner](https://github.com/TTK4145/Project-resources/tree/master/cost_fns/hall_request_assigner)