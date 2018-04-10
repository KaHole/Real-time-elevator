#!/usr/bin/env bash

#nodes=( one two three four )
nodes=( one )
port=15657

for node in "${nodes[@]}"
do
    osascript -e "tell app \"Terminal\" to do script [\"cd ${PWD}/../apps && ./SimElevatorServer_mac --port $port\"]"
    osascript -e "tell app \"Terminal\" to do script [\"cd ${PWD}/.. && rebar3 shell --config config/$node.config  --sname $node --apps elevator\"]"

    # kan være man bør starte alle simulatorene først

    port=$((port+1))
done
