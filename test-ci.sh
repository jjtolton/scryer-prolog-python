#!/bin/bash
set -e

echo "=== Testing GitHub Actions CI Locally ==="
echo ""
echo "This script runs the same steps as .github/workflows/test.yml"
echo "but in a local Docker container for faster iteration."
echo ""

docker build -f test-ci.Dockerfile -t scrypy-ci-test .

echo ""
echo "=== Docker build successful! All tests passed! ==="
echo ""
echo "To run an interactive shell in the test environment:"
echo "  docker run -it --rm scrypy-ci-test"
