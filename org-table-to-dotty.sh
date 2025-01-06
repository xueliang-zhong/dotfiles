#!/bin/bash
if [ $# -ne 1 ]; then
    echo "Usage: $0 <org_table_file>"
    exit 1
fi

input_file="$1"

# Validate the file exists
if [ ! -f "$input_file" ]; then
    echo "File not found: $input_file"
    exit 1
fi

# Start building the DOT graph
echo "digraph G {"
echo "    rankdir=LR;"
echo "    node [shape=box, style=filled];"

while read LINE_INPUT; do
    col1=$(echo $LINE_INPUT | tr '|' ' ' | awk '{print $1}')
    col2=$(echo $LINE_INPUT | tr '|' ' ' | awk '{print $2}')
    col3=$(echo $LINE_INPUT | tr '|' ' ' | awk '{print $3}')

    # Skip empty lines
    if [[ -z "$col1" ]]; then
      echo
      continue
    fi

    # allow me to colour the nodes with "| # node [color=COLOUR] | | |"
    if [[ "$col1" == *#* ]]; then
      col1=$(echo "$LINE_INPUT" | sed "s/[#|]//g" | sed "s/ [ ]*$//")
      echo "  $col1;"
      continue
    fi

    # Output the dotty connection
    echo "    $col1 -> $col2;"

    # Add URL to the target node if provided
    if [[ -n "$col3" ]]; then
      echo "    $col2 [URL=\"$col3\", fontcolor=blue];"
    fi
done < "$input_file"

echo "}"
