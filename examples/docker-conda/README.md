# Docker Example: Scryer-Python with Conda

This example demonstrates scryer-python integration with Conda/Miniconda environments, including support for NumPy and other C extension packages.

## NumPy and C Extensions Now Supported! 🎉

This example uses a custom build of Scryer Prolog with **RTLD_GLOBAL support**, which enables Python C extensions (NumPy, SciPy, pandas, etc.) to work correctly. The standard Scryer Prolog release loads libraries with `RTLD_LOCAL`, which prevents C extensions from resolving Python API symbols.

**Once the RTLD_GLOBAL PR is merged upstream**, you'll be able to use the official Scryer Prolog releases with NumPy and other scientific Python packages.

## What This Demonstrates

- Installing Miniconda
- Creating a conda environment with Python 3.11
- Installing scientific Python packages (NumPy, requests)
- Using `scryer-python` with conda-managed Python
- Demonstrating `python_executable` option for conda envs
- **NumPy array operations working with RTLD_GLOBAL**

## Quick Start

Build and run the demo:

```bash
# From project root
docker build -f examples/docker-conda/Dockerfile -t scryer-python-conda .
docker run --rm scryer-python-conda
```

**Note**: Building Scryer Prolog from source takes 5-10 minutes.

## What It Does

The demo script:
1. Initializes Python using the conda environment's Python executable
2. Prints Python and conda environment information
3. **Imports and uses NumPy for array operations**
4. Makes an HTTP request to GitHub API using requests
5. Demonstrates scientific Python + Prolog integration

## Quick Python Testing

Test Python with conda environment using one-liners:

```bash
# NumPy array operations
docker run --rm scryer-python-conda /bin/bash -c "source /app/conda_env.sh && scryer-prolog -g \"use_module('src/lib/python'), py_initialize([shared_library_path('/opt/conda/envs/myenv/lib/libpython3.11.so'), python_executable('/opt/conda/envs/myenv/bin/python')]), py_run_simple_string(\\\"import numpy as np; arr = np.array([1,2,3,4,5]); print(f'Array: {arr}, Sum: {arr.sum()}')\\\"), py_finalize, halt\""

# NumPy statistics
docker run --rm scryer-python-conda /bin/bash -c "source /app/conda_env.sh && scryer-prolog -g \"use_module('src/lib/python'), py_initialize([shared_library_path('/opt/conda/envs/myenv/lib/libpython3.11.so'), python_executable('/opt/conda/envs/myenv/bin/python')]), py_run_simple_string(\\\"import numpy as np; arr = np.array([1,2,3,4,5]); print(f'Mean: {arr.mean()}, Std: {arr.std()}')\\\"), py_finalize, halt\""

# HTTP request with requests library
docker run --rm scryer-python-conda /bin/bash -c "source /app/conda_env.sh && scryer-prolog -g \"use_module('src/lib/python'), py_initialize([shared_library_path('/opt/conda/envs/myenv/lib/libpython3.11.so'), python_executable('/opt/conda/envs/myenv/bin/python')]), py_run_simple_string(\\\"import requests; r = requests.get('https://httpbin.org/json'); print(f'Status: {r.status_code}')\\\"), py_finalize, halt\""
```

Or for interactive exploration:

```bash
docker run --rm -it scryer-python-conda /bin/bash
# Then run: source /app/conda_env.sh && scryer-prolog
```

## Expected Output

```
Python version: 3.11.x ...
Python prefix: /opt/conda/envs/myenv
Conda env: /opt/conda/envs/myenv

NumPy version: X.XX.X
NumPy array sum: 15

Scryer Prolog GitHub stars: XXX

🎉 Scryer-Python + Conda + NumPy: SUCCESS!
```

## Dockerfile Breakdown

1. **Base Image**: Ubuntu 22.04
2. **System Dependencies**: curl, git, build tools, pkg-config
3. **Miniconda**: Installed to `/opt/conda`, configured to use conda-forge channel
4. **Conda Environment**: `myenv` with Python 3.11, NumPy, requests
5. **Scryer Prolog**: Built from source with RTLD_GLOBAL support
6. **Demo Script**: Shows conda + NumPy integration

## Key Points

- Uses `python_executable` option pointing to conda env's Python
- Python automatically finds packages in the conda environment
- No need for `python_home` when using `python_executable`
- Demonstrates scientific Python ecosystem integration
- **NumPy and C extension packages work with RTLD_GLOBAL support**

## Conda vs UV

**Why use Conda?**
- Familiar to data scientists
- Can manage non-Python dependencies
- Large ecosystem of pre-built packages

**Why use UV?** (see `examples/docker/`)
- Much faster package installation
- Smaller image size
- Simpler setup
- **Same C extension limitation applies**

Currently, for pure-Python packages, UV is recommended. Both have the same RTLD_GLOBAL limitation.

## Customizing

Add more pure-Python packages:

```dockerfile
RUN conda create -n myenv python=3.11 --override-channels -c conda-forge -y && \
    conda run -n myenv pip install requests beautifulsoup4 flask
```

**Note**: Avoid packages with C extensions until Scryer FFI adds RTLD_GLOBAL support.

Or use pip in the conda environment:

```dockerfile
RUN conda run -n myenv pip install tensorflow torch transformers
```

## Running Your Own Script

Mount it as a volume:

```bash
docker run --rm -v $(pwd)/my_script.pl:/app/my_script.pl \
    scryer-python-conda \
    /bin/bash -c "source /app/conda_env.sh && scryer-prolog my_script.pl"
```
