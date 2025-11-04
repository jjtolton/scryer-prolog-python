# Python Environment Managers Guide

This guide explains how to use Scryer Prolog's Python integration with different Python environment managers, including how to compile Python with shared library support and configure environment variables.

## Table of Contents

- [System Python](#system-python)
- [pyenv](#pyenv)
- [Conda/Miniconda](#condaminiconda)
- [virtualenv](#virtualenv)
- [uv](#uv)
- [Environment Variables](#environment-variables)

---

## System Python

System Python installations (via `apt`, `brew`, etc.) usually include shared libraries by default.

### Linux (Ubuntu/Debian)

```bash
# Install Python with development libraries
sudo apt-get install python3.11 python3.11-dev libpython3.11
```

The shared library will be at:
```
/usr/lib/x86_64-linux-gnu/libpython3.11.so
```

### macOS (Homebrew)

```bash
# Install Python (includes shared library)
brew install python@3.11
```

Shared library locations:
- **Apple Silicon**: `/opt/homebrew/lib/libpython3.11.dylib`
- **Intel**: `/usr/local/lib/libpython3.11.dylib`

**Auto-detection**: Scryer Prolog automatically detects system Python installations in these standard locations.

---

## pyenv

[pyenv](https://github.com/pyenv/pyenv) is a Python version manager that lets you easily switch between multiple versions of Python.

### Building Python with Shared Library Support

**By default, pyenv builds Python WITHOUT shared library support.** You must explicitly enable it:

```bash
# Set the configuration flag before installing
env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install 3.11.5

# Or using the alternate syntax
CONFIGURE_OPTS=--enable-shared pyenv install 3.11.5
```

### Finding the Shared Library

After installation with `--enable-shared`:

```bash
# The library will be in the version's lib directory
~/.pyenv/versions/3.11.5/lib/libpython3.11.so  # Linux
~/.pyenv/versions/3.11.5/lib/libpython3.11.dylib  # macOS
```

### Configuration

#### Method 1: Using `python_config.pl`

```bash
cp python_config.pl.example python_config.pl
```

Edit `python_config.pl`:
```prolog
python_library_path_config('/home/user/.pyenv/versions/3.11.5/lib/libpython3.11.so').
```

#### Method 2: Using environment variable

```bash
export LIBPYTHON_PATH="$HOME/.pyenv/versions/3.11.5/lib/libpython3.11.so"
scryer-prolog examples/python_demo.pl
```

### LD_LIBRARY_PATH (Linux/macOS)

On Linux and macOS, you may need to set `LD_LIBRARY_PATH` (Linux) or `DYLD_LIBRARY_PATH` (macOS) so the system can find the Python shared library at runtime:

```bash
# Linux
export LD_LIBRARY_PATH="$HOME/.pyenv/versions/3.11.5/lib:$LD_LIBRARY_PATH"

# macOS
export DYLD_LIBRARY_PATH="$HOME/.pyenv/versions/3.11.5/lib:$DYLD_LIBRARY_PATH"
```

**Tip**: Add this to your `.bashrc` or `.zshrc` if you use pyenv regularly.

---

## Conda/Miniconda

[Conda](https://docs.conda.io/) is a popular package and environment manager, especially for data science workflows.

### Shared Library Support

**Conda Python installations include shared libraries by default**, so no special compilation is needed.

### Finding the Shared Library

```bash
# Activate your conda environment
conda activate myenv

# Find the library path
python3-config --prefix
# Output: /home/user/miniconda3/envs/myenv

# The shared library will be at:
# Linux: /home/user/miniconda3/envs/myenv/lib/libpython3.11.so
# macOS: /home/user/miniconda3/envs/myenv/lib/libpython3.11.dylib
```

### Configuration

#### Method 1: Using `python_config.pl`

```bash
cp python_config.pl.example python_config.pl
```

Edit `python_config.pl`:
```prolog
% Replace with your actual conda environment path
python_library_path_config('/home/user/miniconda3/envs/myenv/lib/libpython3.11.so').
```

#### Method 2: Using environment variable

```bash
conda activate myenv
export LIBPYTHON_PATH="$(python3-config --prefix)/lib/libpython3.11.so"
scryer-prolog examples/python_demo.pl
```

### LD_LIBRARY_PATH (CRITICAL for Conda!)

**Conda environments REQUIRE setting `LD_LIBRARY_PATH` (Linux) or `DYLD_LIBRARY_PATH` (macOS).**

This is because Conda's shared libraries are not in standard system locations.

```bash
# Activate environment
conda activate myenv

# Linux - REQUIRED
export LD_LIBRARY_PATH="$(python3-config --prefix)/lib:$LD_LIBRARY_PATH"

# macOS - REQUIRED
export DYLD_LIBRARY_PATH="$(python3-config --prefix)/lib:$DYLD_LIBRARY_PATH"

# Now run Scryer Prolog
scryer-prolog examples/python_demo.pl
```

### Conda Launch Script Example

Create a script `run-scryer-conda.sh`:

```bash
#!/bin/bash

# Activate conda environment
source activate myenv

# Set library path (REQUIRED!)
export LD_LIBRARY_PATH="$(python3-config --prefix)/lib:$LD_LIBRARY_PATH"

# Run Scryer Prolog
scryer-prolog "$@"
```

Make it executable:
```bash
chmod +x run-scryer-conda.sh
./run-scryer-conda.sh examples/python_demo.pl
```

### Apple Silicon macOS + Conda

The issue reporter ([#1](https://github.com/jjtolton/scryer-prolog-python/issues/1)) encountered problems on Apple Silicon with Conda. If you experience similar issues:

1. Verify the library architecture matches:
   ```bash
   file $(python3-config --prefix)/lib/libpython3.12.dylib
   # Should show: Mach-O 64-bit dynamically linked shared library arm64
   ```

2. Ensure `DYLD_LIBRARY_PATH` is set:
   ```bash
   export DYLD_LIBRARY_PATH="$(python3-config --prefix)/lib:$DYLD_LIBRARY_PATH"
   ```

3. Try using `python_config.pl` to explicitly specify the path.

4. If FFI signature mismatches occur, please report them as an issue with details about your platform and Python version.

---

## virtualenv

[virtualenv](https://virtualenv.pypa.io/) creates isolated Python environments but **uses the Python installation from your system or pyenv**.

### Important Note

**virtualenv does NOT compile Python** - it creates a lightweight copy of an existing Python installation. Therefore:

- If you're using **system Python**: The shared library is already available.
- If you're using **pyenv**: You must have installed Python with `--enable-shared` (see [pyenv section](#pyenv)).

### Using virtualenv with Scryer Prolog

```bash
# Create virtual environment (uses system Python)
python3 -m venv myenv

# Or with pyenv's Python
~/.pyenv/versions/3.11.5/bin/python -m venv myenv

# Activate
source myenv/bin/activate
```

### Configuration

virtualenv uses the same shared library as its base Python installation:

- **System Python**: Auto-detection should work.
- **pyenv Python**: Configure as shown in the [pyenv section](#pyenv).

No additional configuration is typically needed for virtualenv itself.

---

## uv

[uv](https://github.com/astral-sh/uv) is a fast Python package and project manager written in Rust. It can manage Python versions, create environments, and handle dependencies.

### Python Installations

uv can install Python versions with `uv python install`:

```bash
# Install Python 3.11
uv python install 3.11

# List installed versions
uv python list
```

### Finding uv-Managed Python Libraries

uv stores Python installations in a platform-specific location:

```bash
# Linux
~/.local/share/uv/python/cpython-3.11.*/lib/libpython3.11.so

# macOS
~/Library/Application Support/uv/python/cpython-3.11.*/lib/libpython3.11.dylib
```

Use `uv python dir` to find the installation directory:

```bash
uv python dir 3.11
# Output: /home/user/.local/share/uv/python/cpython-3.11.5-linux-x86_64-gnu
```

### Configuration

#### Method 1: Using `python_config.pl`

```bash
cp python_config.pl.example python_config.pl
```

Edit `python_config.pl`:
```prolog
% Get the path from: uv python dir 3.11
python_library_path_config('/home/user/.local/share/uv/python/cpython-3.11.5-linux-x86_64-gnu/lib/libpython3.11.so').
```

#### Method 2: Using environment variable

```bash
UV_PYTHON_DIR=$(uv python dir 3.11)
export LIBPYTHON_PATH="$UV_PYTHON_DIR/lib/libpython3.11.so"
scryer-prolog examples/python_demo.pl
```

### LD_LIBRARY_PATH

Similar to Conda, you may need to set the library path:

```bash
# Linux
export LD_LIBRARY_PATH="$(uv python dir 3.11)/lib:$LD_LIBRARY_PATH"

# macOS
export DYLD_LIBRARY_PATH="$(uv python dir 3.11)/lib:$DYLD_LIBRARY_PATH"
```

---

## Environment Variables

### LIBPYTHON_PATH

Scryer Prolog checks this environment variable to locate the Python shared library:

```bash
export LIBPYTHON_PATH="/path/to/libpython3.11.so"
scryer-prolog examples/python_demo.pl
```

This overrides auto-detection and takes precedence over `python_config.pl`.

### LD_LIBRARY_PATH (Linux) / DYLD_LIBRARY_PATH (macOS)

These environment variables tell the dynamic linker where to find shared libraries at runtime.

**When to set them:**
- ✅ **Always needed** for Conda environments
- ✅ **Usually needed** for pyenv installations
- ✅ **Sometimes needed** for uv-managed Python
- ❌ **Not needed** for system Python (already in standard locations)

**How to set:**

```bash
# Linux
export LD_LIBRARY_PATH="/path/to/python/lib:$LD_LIBRARY_PATH"

# macOS (Intel or Apple Silicon)
export DYLD_LIBRARY_PATH="/path/to/python/lib:$DYLD_LIBRARY_PATH"
```

**Making it permanent:**

Add to your shell configuration file (`~/.bashrc`, `~/.zshrc`, etc.):

```bash
# Example for pyenv
export LD_LIBRARY_PATH="$HOME/.pyenv/versions/3.11.5/lib:$LD_LIBRARY_PATH"
```

---

## Configuration Priority

Scryer Prolog searches for the Python library in this order:

1. **`python_config.pl`** - User configuration file (highest priority)
2. **`LIBPYTHON_PATH`** - Environment variable
3. **Auto-detection** - Searches common system locations

If all three fail, you'll see an error message with setup instructions.

---

## Troubleshooting

### "Could not find Python shared library"

1. Verify the library exists:
   ```bash
   ls -l /path/to/libpython3.11.so
   ```

2. Check if Python was built with `--enable-shared` (for pyenv):
   ```bash
   ldd ~/.pyenv/versions/3.11.5/bin/python | grep libpython
   # Should show libpython3.11.so path
   ```

3. Try explicit configuration with `python_config.pl` or `LIBPYTHON_PATH`.

### "initialization/1 failed" or FFI errors (macOS)

If you see errors like in [issue #1](https://github.com/jjtolton/scryer-prolog-python/issues/1):

1. Set `DYLD_LIBRARY_PATH`:
   ```bash
   export DYLD_LIBRARY_PATH="$(python3-config --prefix)/lib:$DYLD_LIBRARY_PATH"
   ```

2. Verify architecture matches:
   ```bash
   file $(which scryer-prolog)
   file /path/to/libpython3.12.dylib
   # Both should show arm64 or both should show x86_64
   ```

3. Try a different Python installation (e.g., Homebrew instead of Conda).

### "undefined symbol" errors

This usually means the dynamic linker can't find the Python library. Set `LD_LIBRARY_PATH` or `DYLD_LIBRARY_PATH`.

---

## Quick Reference

| Environment Manager | Needs `--enable-shared`? | Needs `LD_LIBRARY_PATH`? | Auto-Detected? |
|---------------------|--------------------------|--------------------------|----------------|
| System Python       | No (included)            | No                       | ✅ Yes         |
| pyenv               | **Yes** (must specify)   | Usually                  | ❌ No          |
| Conda/Miniconda     | No (included)            | **Yes** (required)       | ❌ No          |
| virtualenv          | Inherits from base       | Inherits from base       | Depends on base |
| uv                  | No (included)            | Usually                  | ❌ No          |

---

## Getting Help

If you encounter issues:

1. Check this guide for your environment manager.
2. Review [INSTALL.md](../INSTALL.md) for general installation instructions.
3. See [README.md](../README.md#requirements) for requirements.
4. Open an issue at https://github.com/jjtolton/scryer-prolog-python/issues with:
   - Your OS and architecture (e.g., macOS 15.3 arm64)
   - Python version and environment manager
   - Error messages
   - Output of `file /path/to/libpython3.X.so`
