#!/bin/sh

./scripts/test.ros 2>&1 | grep -v -E '^(Backtrace|[0-9]+:)'