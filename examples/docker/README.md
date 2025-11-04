# Docker Example: Scryer-Python Complete Setup

This example demonstrates a complete end-to-end setup of scryer-python in Docker, from scratch.

## What This Demonstrates

- Installing Scryer Prolog
- Installing Python with `uv`
- Creating a virtual environment
- Installing Python packages (`requests`)
- Using `scryer-python` to call Python from Prolog
- Making HTTP requests from Python via Prolog

## Quick Start

Build and run the demo:

```bash
# From project root
docker build -f examples/docker/Dockerfile -t scryer-python-demo .
docker run --rm scryer-python-demo
```

**Note**: Building Scryer Prolog from source takes 5-10 minutes. For faster local testing without Docker, see the main project README.

## What It Does

The demo script:
1. Initializes Python using the venv's Python executable
2. Prints Python version information
3. Imports and uses the `requests` library
4. Makes an API call to GitHub to get Scryer Prolog's star count
5. Demonstrates that everything works together seamlessly

## Expected Output

```
Python version: 3.11.14 (main, Oct 10 2025, 12:48:11) [Clang 20.1.4 ]
Python prefix: /app/.venv

Requests version: 2.32.5

Scryer Prolog GitHub stars: XXX

🎉 Scryer-Python Docker Demo: SUCCESS!
```

## Dockerfile Breakdown

1. **Base Image**: Ubuntu 22.04
2. **System Dependencies**: curl, git, build tools
3. **uv Installation**: Fast Python package manager
4. **Python 3.11.14**: Installed via uv
5. **Scryer Prolog**: Downloaded from official releases
6. **Virtual Environment**: Created with uv
7. **Python Packages**: Requests installed in venv
8. **Demo Script**: Shows complete integration

## Key Points

- Uses `python_executable` option to point to venv's Python
- Python automatically finds packages in the venv (no manual sys.path manipulation)
- Demonstrates real-world usage with HTTP requests
- Self-contained and reproducible

## Customizing

To add more Python packages, modify the Dockerfile:

```dockerfile
RUN uv pip install requests numpy pandas
```

To run your own Prolog script, mount it as a volume:

```bash
docker run --rm -v $(pwd)/my_script.pl:/app/my_script.pl scryer-python-demo scryer-prolog my_script.pl
```
