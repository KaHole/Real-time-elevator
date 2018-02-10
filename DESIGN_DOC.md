
# Design Review

![alt text](https://github.com/TTK4145/exercise4-wrong_on_so_many_levels/raw/master/diagrams/design-review.jpeg "Meme")

---

## Technology
* Erlang

* Distributed nodes (TCP)
  * Heartbeats / watchdog (system monitoring)
  
* UDP for node discovery

---

## Messages
* Broadcast local state to all other nodes upon changes.

* Send a `record` containing the state.

---

## Orders
A set of orders is a certain state in which the elevator system can be.

* Orders are to be determined by analyzing world state.

* A deterministic algorithm to decide who's accepting the order.
  * Even though some nodes haven't received the order-inducing state yet.
  * Because world state is an aggregate of the nodes' internal state:
    * Worst case; an order doesn't get accepted on first try
    * The order will not cease to exist and will be picked up next round.
    
* Order identifier: `elevator_n:floor_n:up` to identify accepted orders.

---

## Network
![alt text](https://github.com/TTK4145/exercise4-wrong_on_so_many_levels/raw/master/diagrams/elevator_gruppe19.png "Network diagram")

---

## Incoming state

![alt text](https://github.com/TTK4145/exercise4-wrong_on_so_many_levels/blob/master/diagrams/InOrderDiagram.png?raw=true "Incoming Order diagram")

---

## Error handling

* Erlang ensures packet delivery

* Watchdog ensures order completion

---
