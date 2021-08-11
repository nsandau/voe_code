#!/bin/bash

# outcome, protocol, n_splits

# BIN 
## NO PROTOCOL
bash run_splits.sh bin none 15 # passende 

## Handoll split
bash run_splits.sh bin handoll 3 # untested. decreased from 5

## WITH PROTOCOLS
for protocol in beks skou; do
bash run_splits.sh bin $protocol 1
done

# FUNC 
## NO PROTOCOL
bash run_splits.sh func none 8 # passende

## HANDOLL
bash run_splits.sh func handoll 2 # untested. decreased from 5

## WITH PROTOCOLS 
for protocol in beks skou; do
bash run_splits.sh func $protocol 1
done

# QOL 
## ALL # beks doesnt include qol
for protocol in none handoll skou; do
bash run_splits.sh qol $protocol 1
done


