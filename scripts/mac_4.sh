#!/usr/bin/env bash

# nodes=( one two three four )
nodes=( one )
port=15657

for node in "${nodes[@]}"
do
    osascript -e "tell app \"Terminal\" to do script [\"cd ${PWD}/../apps && ./SimElevatorServer_mac --port $port\"]"
    osascript -e "tell app \"Terminal\" to do script [\"cd ${PWD}/.. && rebar3 shell --config config/$node.config  --sname $node --apps elevator\"]"

    #linux
    # gnome-terminal -x sh -c "cd ${PWD}/../apps; ./SimElevatorServer --port $port; exec bash"
    # gnome-terminal -x sh -c "cd ${PWD}/..; rebar3 shell --sname $node --apps elevator; exec bash"

    port=$((port+1))
done
