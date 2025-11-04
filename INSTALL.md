# Installation Guide

## Prerequisites

### 1. Scryer Prolog

Install Scryer Prolog v0.10.0 or later:

```bash
# From source
git clone https://github.com/mthom/scryer-prolog
cd scryer-prolog
cargo build --release

# The binary will be at target/release/scryer-prolog
# Add it to your PATH or create a symlink
```

See the [Scryer Prolog installation guide](https://github.com/mthom/scryer-prolog#installation) for more details.

### 2. Python Shared Library

This library requires Python 3.10+ with the **shared library** (`.so` on Linux, `.dylib` on macOS, `.dll` on Windows).

#### Linux (Ubuntu/Debian)

Install Python development libraries:

```bash
# For Python 3.10
sudo apt-get install python3.10 python3.10-dev libpython3.10

# For Python 3.11
sudo apt-get install python3.11 python3.11-dev libpython3.11

# For Python 3.12
sudo apt-get install python3.12 python3.12-dev libpython3.12
```

Verify the shared library exists:

```bash
# For Python 3.10
ls -l /usr/lib/x86_64-linux-gnu/libpython3.10.so

# For Python 3.11
ls -l /usr/lib/x86_64-linux-gnu/libpython3.11.so

# For Python 3.12
ls -l /usr/lib/x86_64-linux-gnu/libpython3.12.so
```

#### macOS

Python installed via Homebrew typically includes the shared library:

```bash
brew install python@3.11

# Verify shared library location
ls -l /opt/homebrew/lib/libpython3.11.dylib
# or on Intel Macs:
ls -l /usr/local/lib/libpython3.11.dylib
```

#### Finding Your Python Shared Library

If you're not sure where your Python shared library is located:

```bash
# Linux
find /usr/lib -name "libpython3.*.so*" 2>/dev/null

# macOS
find /usr/local/lib /opt/homebrew/lib -name "libpython3.*.dylib" 2>/dev/null
```

Common locations:
- **Linux (system Python)**: `/usr/lib/x86_64-linux-gnu/libpython3.X.so`
- **Linux (pyenv)**: `~/.pyenv/versions/3.X.X/lib/libpython3.X.so`
- **macOS (Homebrew, Apple Silicon)**: `/opt/homebrew/lib/libpython3.X.dylib`
- **macOS (Homebrew, Intel)**: `/usr/local/lib/libpython3.X.dylib`

## Installation

1. Clone this repository:

```bash
git clone https://github.com/jjtolton/scryer-prolog-python.git
cd scryer-prolog-python
```

2. **For system Python**: Auto-detection should work immediately.
   **For other environments** (pyenv, Conda, uv): See the [Configuration](#configuration) section.

3. Test the installation:

```bash
scryer-prolog examples/python_demo.pl
```

## Configuration

Scryer Prolog searches for the Python library in this order:

1. **`python_config.pl`** configuration file (highest priority)
2. **`LIBPYTHON_PATH`** environment variable
3. **Auto-detection** from common system locations

If you're using **system Python** (via `apt` or `brew`), auto-detection should work automatically. For other Python installations, choose one of the methods below.

### Method 1: Configuration File (Recommended for Permanent Setup)

Create a `python_config.pl` file to specify your Python library location:

```bash
# Copy the example configuration
cp python_config.pl.example python_config.pl

# Edit python_config.pl and uncomment/modify the path
```

Example `python_config.pl`:

```prolog
% Conda environment
python_library_path_config('/home/user/miniconda3/envs/myenv/lib/libpython3.11.so').

% Or pyenv
python_library_path_config('/home/user/.pyenv/versions/3.11.5/lib/libpython3.11.so').

% Or uv
python_library_path_config('/home/user/.local/share/uv/python/cpython-3.11.5-linux-x86_64-gnu/lib/libpython3.11.so').
```

**Note**: `python_config.pl` is git-ignored, so your local configuration won't be committed.

### Method 2: Environment Variable (Good for Temporary/Per-Session)

Set the `LIBPYTHON_PATH` environment variable:

```bash
export LIBPYTHON_PATH=/path/to/libpython3.11.so
scryer-prolog examples/python_demo.pl
```

For Conda environments:
```bash
conda activate myenv
export LIBPYTHON_PATH="$(python3-config --prefix)/lib/libpython3.11.so"
scryer-prolog examples/python_demo.pl
```

### Method 3: Auto-Detection (System Python Only)

If you're using system Python installed via `apt-get` or `brew`, the library should be auto-detected from these locations:

- **Linux**: `/usr/lib/x86_64-linux-gnu/libpython3.{12,11,10}.so`
- **macOS (Apple Silicon)**: `/opt/homebrew/lib/libpython3.{12,11,10}.dylib`
- **macOS (Intel)**: `/usr/local/lib/libpython3.{12,11,10}.dylib`

No configuration needed!

## Python Environment Managers

**See [docs/PYTHON_ENVIRONMENTS.md](docs/PYTHON_ENVIRONMENTS.md) for comprehensive guides on:**

- **pyenv**: Building Python with `--enable-shared`
- **Conda/Miniconda**: Setting `LD_LIBRARY_PATH` (required!)
- **virtualenv**: Configuration based on base Python
- **uv**: Finding uv-managed Python installations

### Quick: LD_LIBRARY_PATH / DYLD_LIBRARY_PATH

Some Python installations (especially **Conda** and **pyenv**) require setting the library search path:

**Linux:**
```bash
export LD_LIBRARY_PATH="/path/to/python/lib:$LD_LIBRARY_PATH"
```

**macOS:**
```bash
export DYLD_LIBRARY_PATH="/path/to/python/lib:$DYLD_LIBRARY_PATH"
```

**Example for Conda (Linux):**
```bash
conda activate myenv
export LD_LIBRARY_PATH="$(python3-config --prefix)/lib:$LD_LIBRARY_PATH"
scryer-prolog examples/python_demo.pl
```

**When is this needed?**
- ✅ Always for Conda
- ✅ Usually for pyenv
- ✅ Sometimes for uv
- ❌ Not needed for system Python

## Troubleshooting

### "Foreign module not found" Error

**Problem**: Scryer Prolog can't find the Python shared library.

**Solution**:
1. Verify the library exists at the path specified in `src/lib/python.pl`
2. Check that you have the `-dev` package installed (`python3.X-dev`)
3. Try using the absolute path from `find` command above

### "undefined symbol" Errors

**Problem**: Wrong Python version or incompatible library.

**Solution**:
1. Ensure the Python version in `src/lib/python.pl` matches your installed version
2. Reinstall the Python development package: `sudo apt-get install --reinstall python3.X-dev`

### FFI Hangs or Segfaults

**Problem**: Incorrect FFI function signatures or memory management issues.

**Solution**:
1. Ensure you're using a compatible Python version (3.10+)
2. Check that you have the full Python development libraries installed
3. Try a minimal test:
   ```bash
   scryer-prolog -g "use_module('src/lib/python'), py_initialize, py_finalize, halt"
   ```

### Import Errors in Python Code

**Problem**: Python packages not found when running `py_run_simple_string`.

**Solution**:
1. Install packages in the system Python that matches your library:
   ```bash
   # For Python 3.10
   python3.10 -m pip install numpy pandas

   # For Python 3.11
   python3.11 -m pip install numpy pandas
   ```

2. Or use a virtual environment and point to that Python's shared library

## Verifying Installation

Run this quick test to verify everything works:

```bash
scryer-prolog << 'EOF'
:- use_module('src/lib/python').
:- initialization(test_python).

test_python :-
    py_initialize,
    py_run_simple_string('import sys'),
    py_run_simple_string('print(f"Python {sys.version}")'),
    py_run_simple_string('print("Successfully loaded!")'),
    py_finalize,
    halt.
EOF
```

You should see:
```
Python 3.10.X (or your version)
Successfully loaded!
```

## Next Steps

See the [README.md](README.md) for:
- Quick start guide
- API reference
- Examples
- Current research questions and project status
