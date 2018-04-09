elevator project (電梯)
=====
[![forthebadge](https://forthebadge.com/images/badges/fuck-it-ship-it.svg)](https://forthebadge.com) [![Build Status](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels.svg?token=VaZNdDabsDWKmoAAY6fP&branch=master)](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels)

Real-time elevator system for TTK4145

Modules
----

- [ ] Gå over kriterier for evaluering og spec for å få god karakter
- [ ] Heartbeats / fault-recovery / tolerance

- [ ] `erl_tidy` kan brukes for formattering og optimalisering
- [ ] Style guide: https://github.com/inaka/erlang_guidelines

- [x] Elevator logic
- [x] Coordinator (main)
- [x] Consensus state machine
- [x] Hall request assigner integration
- [x] Networking
- [x] Node discovery
- [x] Elevator driver (premade)

Release
----
    $ rebar3 release

Notes on generating binary:

- A release on linux will run on linux (no cross-platform releases)
- No config files allowed, ikke noe problem egentlig, det bruker vi kun til testing
- `include_erts` gjør at erlang ikke trenger å være installert, og sikrer riktig kjøring uansett versjon på maskinen
- 

----

Build & Run
----
    $ rebar3 shell --sname one --apps elevator

Build
----
    $ rebar3 compile


![Elevator diagram](https://github.com/TTK4145/project-wrong_on_so_many_levels/raw/master/doc/Sanntid_moduler.png)

----

Learning Resources
-----

http://erlangbyexample.org/

http://learnyousomeerlang.com/content

https://learnxinyminutes.com/docs/erlang/

https://www.rebar3.org/

Consensus reading
-----

https://raft.github.io/

https://en.wikipedia.org/wiki/State_machine_replication

https://people.cs.umass.edu/~arun/cs677/notes/week6.pdf

http://www.cs.cornell.edu/fbs/publications/SMSurvey.pdf

