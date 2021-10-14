#!/bin/bash

# outcome, protocol, n_splits

# BIN 
## NO PROTOCOL
bash run_splits.sh bin none 15 # passende 

## HANDOL AND RCT split
for protocol in handoll rct; do
bash run_splits.sh bin $protocol 3 
done

## BEKS AND SKOU
for protocol in beks skou; do
bash run_splits.sh bin $protocol 1
done

# FUNC 
## NO PROTOCOL
bash run_splits.sh func none 8 # passende

## HANDOLL AND RCT
for protocol in handoll rct; do
bash run_splits.sh func $protocol 2 
done

## SKOU BEKS 
for protocol in beks skou; do
bash run_splits.sh func $protocol 1
done

# QOL 
## ALL # beks doesnt include qol
for protocol in none handoll skou rct; do
bash run_splits.sh qol $protocol 1
done


