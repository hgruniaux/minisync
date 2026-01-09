#!/bin/bash

shopt -s globstar

DO_NOT_BUILD_COMPILER=0
SHOW_PASSED=1
UPDATE_EXPECTED=0

# Parse options
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --no-build) DO_NOT_BUILD_COMPILER=1 ;;
        --skip-passed) SHOW_PASSED=0 ;;
        --update) UPDATE_EXPECTED=1 ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# Build the compiler, and exit if we can not build it.
if [ "$DO_NOT_BUILD_COMPILER" -eq 0 ]; then
  dune build bin/main.exe
  if [ $? -ne 0 ]; then
      echo -e "[\x1b[31mFAILED\x1b[0m] Build failed."
      exit 1
  fi
fi

MINISYNC="./_build/default/bin/main.exe --check"

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
FATAL_TESTS=0

# Mark the test as passed.
pass () {
  if [ "$SHOW_PASSED" -eq 1 ]; then
    echo -e "[\x1b[32mPASSED\x1b[0m] $1."
  fi
  PASSED_TESTS=$((PASSED_TESTS + 1))
  TOTAL_TESTS=$((TOTAL_TESTS + 1))
}

# Mark the test as failed.
fail () {
  echo -e "[\x1b[31mFAILED\x1b[0m] $1."
  FAILED_TESTS=$((FAILED_TESTS + 1))
  TOTAL_TESTS=$((TOTAL_TESTS + 1))
}

# Mark the test as fatal fail (for example if our compiler crashed).
fatal () {
  echo -e "[\x1b[1;35m!FATAL\x1b[0m] $1 (crashed)."
  FATAL_TESTS=$((FATAL_TESTS + 1))
  TOTAL_TESTS=$((TOTAL_TESTS + 1))
}

# Run a given test.
run_test () {
  local file="$1"
  local expected_exit="$2"

  OUTPUT_FILE=$(mktemp)
  $MINISYNC --color=never "$file" > "$OUTPUT_FILE" 2>&1
  local exit_code=$?
  sed -i "s|$file|dummy.sync|g" "$OUTPUT_FILE"

  if [ $exit_code -eq $expected_exit ]; then
    local expected_file="${file%.sync}.expected"
    if [ -f "$expected_file" ]; then
      if diff -q "$OUTPUT_FILE" "$expected_file" >/dev/null; then
        pass "$file"
      else
        if [ "$UPDATE_EXPECTED" -eq 1 ]; then
          cp "$OUTPUT_FILE" "$expected_file"
          echo -e "[\x1b[33mUPDATED\x1b[0m] $expected_file."
          PASSED_TESTS=$((PASSED_TESTS + 1))
          TOTAL_TESTS=$((TOTAL_TESTS + 1))
        else
          fail "$file (output mismatch)"
          # Optional: show diff
          diff "$expected_file" "$OUTPUT_FILE"
        fi
      fi
    else
      pass "$file"
    fi
  elif [ $expected_exit -eq 1 ] && [ $exit_code -eq 0 ]; then
     fail "$file"
  elif [ $expected_exit -eq 0 ] && [ $exit_code -eq 1 ]; then
     fail "$file"
  else
     fatal "$file"
  fi
  rm "$OUTPUT_FILE"
}

# Tests in the fail directory must fail typechecking
for file in test/syntax/fail/**/*.sync; do
  run_test "$file" 1
done

# Tests in the pass directory must pass typechecking
for file in test/syntax/pass/**/*.sync; do
  run_test "$file" 0
done

run_autocomplete_test () {
  # Find the position of the AUTOCOMPLETE marker <|>
  local file="$1"

  # Use awk to find the line (NR) and 1-based column index of the "<|>" marker.
  # "read" assigns the output to the two variables.
  read line_number col_number <<< $(awk '{ idx = index($0, "<|>"); if (idx > 0) { print NR, idx; exit } }' "$file")

  if [ -z "$line_number" ]; then
    echo "No AUTOCOMPLETE marker found in $file"
    fatal "$file"
    return
  fi

  # Create a temporary file, removing the <|> marker
  local temp_file=$(mktemp --suffix=.sync)
  sed 's/<|>//' "$file" > "$temp_file"

  OUTPUT_FILE=$(mktemp)
  # Pass the calculated line and column to the compiler
  $MINISYNC --color=never --autocomplete="$line_number:$col_number" "$temp_file" > "$OUTPUT_FILE" 2>&1
  local exit_code=$?

  # Check against an .expected file (similar to run_test logic)
  local expected_file="${file%.sync}.expected"
  if [ -f "$expected_file" ]; then
    if diff -q "$OUTPUT_FILE" "$expected_file" >/dev/null; then
      pass "$file"
    else
      if [ "$UPDATE_EXPECTED" -eq 1 ]; then
        cp "$OUTPUT_FILE" "$expected_file"
        echo -e "[\x1b[33mUPDATED\x1b[0m] $expected_file."
        PASSED_TESTS=$((PASSED_TESTS + 1))
        TOTAL_TESTS=$((TOTAL_TESTS + 1))
      else
        fail "$file (output mismatch)"
        diff "$expected_file" "$OUTPUT_FILE"
      fi
    fi
  else
    fatal "$file (no expected file found)"
  fi

  rm "$OUTPUT_FILE" "$temp_file"
}

for file in test/autocomplete/**/*.sync; do
  run_autocomplete_test "$file"
done

# Print tests summary.
echo -e "In total, $TOTAL_TESTS were run, out of which:"
echo -e "  \x1b[32m$PASSED_TESTS\x1b[0m ($((PASSED_TESTS * 100 / TOTAL_TESTS))%) passed,"
echo -e "  \x1b[31m$FAILED_TESTS\x1b[0m ($((FAILED_TESTS * 100 / TOTAL_TESTS))%) failed,"
echo -e "  \x1b[1;35m$FATAL_TESTS\x1b[0m ($((FATAL_TESTS * 100 / TOTAL_TESTS))%) fatal (crashed)."
