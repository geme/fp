#!/bin/bash

SBT_OPTS="-Xms512M -Xmx3G -Xss1M"

java $SBT_OPTS -jar `dirname $0`/sbt-launch.jar "$@"
