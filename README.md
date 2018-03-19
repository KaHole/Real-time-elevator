elevator project (電梯)
=====
[![forthebadge](https://forthebadge.com/images/badges/fuck-it-ship-it.svg)](https://forthebadge.com) [![Build Status](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels.svg?token=VaZNdDabsDWKmoAAY6fP&branch=master)](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels)

Real-time elevator system for TTK4145

Modules
----

- [ ] Use ´-type´ syntax to define custom types, and learn how to actually use them. 
- [ ] Coordinator (main)
- [ ] Consensus state machine (50%)
- [ ] Elevator logic

- [ ] Heartbeats / fault-recovery / tolerance

- [x] Hall request assigner integration
- [x] Networking
- [x] Node discovery
- [x] Elevator driver (premade)


Build & Run
----
    $ rebar3 shell --sname one --apps elevator

Build
----
    $ rebar3 compile

Make binary
----
    $ rebar3 release

----

![Elevator diagram](https://github.com/TTK4145/project-wrong_on_so_many_levels/blob/master/doc/elevator_project.png)

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

