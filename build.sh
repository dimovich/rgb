#!/usr/bin/env bash

## Java 8
switchjava8
clj -T:build uber
cp target/rgb.jar release/rgb8.jar


## Java 11
## switchjava11
## clj -T:build uber
## cp target/rgb.jar release/rgb11.jar
