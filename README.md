Real-time elevator system for TTK4145
=====
[![Build Status](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels.svg?token=VaZNdDabsDWKmoAAY6fP&branch=master)](https://travis-ci.com/TTK4145/project-wrong_on_so_many_levels)

# Running the release

1. Start elevator server or simulator on default ports
2. Run the start-script:

    $ ./start.sh

# Development
## Run single elevator
    $ rebar3 shell --sname one --apps elevator
## Compile
    $ rebar3 compile
## Release
    $ rebar3 release

Architecture
----
![Elevator diagram](https://github.com/TTK4145/project-wrong_on_so_many_levels/raw/master/doc/Sanntid_moduler.png)

----

# Built using
- `Erlang`
- [jsone](https://github.com/sile/jsone)
- [Elevator driver](https://github.com/TTK4145/driver-erlang)
- [Hall request assigner](https://github.com/TTK4145/Project-resources/tree/master/cost_fns/hall_request_assigner)
----

![elevator spam](https://media.giphy.com/media/MNepUUKWUjvi0/giphy.gif)
