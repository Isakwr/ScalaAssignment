#!/bin/bash

# Paths to puzzle and solution directories
PUZZLE_DIR="/c/Users/evanm/ikt212g24h/assignments/ValidationData/extended/puzzles"
SOLUTION_DIR="/c/Users/evanm/ikt212g24h/assignments/ValidationData/extended/solutions"
OUTPUT_DIR="C:/Users/evanm/ikt212g24h/assignments/solutions/ScalaAssignment/src/resources"  # Specify a temp directory for the outputs

# Full path to scala-cli
SCALA_CLI_PATH="/c/Users/evanm/AppData/Local/Coursier/data/bin/scala-cli.bat"

# File to store the results
RESULTS_FILE="test_results.csv"

# Initialize results file with headers
echo "Puzzle,Result,Time (ms)" > "$RESULTS_FILE"

# Loop over all the puzzle files
for PUZZLE_FILE in "$PUZZLE_DIR"/*.txt; do
  PUZZLE_NAME=$(basename "$PUZZLE_FILE")

  # Track time for each test with milliseconds precision
  start_time=$(date +%s%3N)

  # Run your project with the puzzle file and store the output using the correct path to scala-cli
  echo "Running puzzle: $PUZZLE_NAME"
  "$SCALA_CLI_PATH" src/*.scala -- "$PUZZLE_FILE" "$OUTPUT_DIR/$PUZZLE_NAME"

  # Check if the output file was generated
  if [ -f "$OUTPUT_DIR/$PUZZLE_NAME" ]; then
    # Compare the output to the expected solution
    SOLUTION_FILE="$SOLUTION_DIR/$PUZZLE_NAME"
    diff_output=$(diff "$OUTPUT_DIR/$PUZZLE_NAME" "$SOLUTION_FILE")

    # Record end time and calculate elapsed time in milliseconds
    end_time=$(date +%s%3N)
    elapsed_time=$((end_time - start_time))

    # Check the result of the diff command
    if [ -z "$diff_output" ]; then
      echo "$PUZZLE_NAME,Passed,$elapsed_time" >> "$RESULTS_FILE"
    else
      echo "$PUZZLE_NAME,Failed (Output mismatch),$elapsed_time" >> "$RESULTS_FILE"
      echo "Differences for $PUZZLE_NAME:"
      echo "$diff_output"
    fi
  else
    # If the output file does not exist
    end_time=$(date +%s%3N)
    elapsed_time=$((end_time - start_time))
    echo "$PUZZLE_NAME,Failed (No output),$elapsed_time" >> "$RESULTS_FILE"
  fi

done
