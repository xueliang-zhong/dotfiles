#!/bin/bash
echo $1 | awk '{print $2}' | xargs google-chrome &> /dev/null
