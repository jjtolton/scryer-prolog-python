# Docker Example: Scryer-Python with Conda

This example demonstrates scryer-python integration with Conda/Miniconda environments.

## What This Demonstrates

- Installing Miniconda
- Creating a conda environment with Python 3.11
- Installing scientific Python packages (NumPy, requests)
- Using `scryer-python` with conda-managed Python
- Demonstrating `python_executable` option for conda envs

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
3. Uses NumPy for array operations
4. Makes an HTTP request to GitHub API using requests
5. Demonstrates scientific Python + Prolog integration

## Quick Python Testing

Test NumPy and scientific Python with one-liners:

```bash
# NumPy array operations
docker run --rm scryer-python-conda /bin/bash -c "source /app/conda_env.sh && scryer-prolog -g \"use_module('src/lib/python'), py_initialize([shared_library_path('/opt/conda/envs/myenv/lib/libpython3.11.so'), python_executable('/opt/conda/envs/myenv/bin/python')]), py_run_simple_string(\\\"import numpy as np; arr = np.array([1,2,3,4,5]); print(f'Array: {arr}')\\\"), py_finalize, halt\""

# NumPy statistics
docker run --rm scryer-python-conda /bin/bash -c "source /app/conda_env.sh && scryer-prolog -g \"use_module('src/lib/python'), py_initialize([shared_library_path('/opt/conda/envs/myenv/lib/libpython3.11.so'), python_executable('/opt/conda/envs/myenv/bin/python')]), py_run_simple_string(\\\"import numpy as np; arr = np.array([1,2,3,4,5]); print(f'Mean: {arr.mean()}, Std: {arr.std()}')\\\"), py_finalize, halt\""

# NumPy matrix multiplication
docker run --rm scryer-python-conda /bin/bash -c "source /app/conda_env.sh && scryer-prolog -g \"use_module('src/lib/python'), py_initialize([shared_library_path('/opt/conda/envs/myenv/lib/libpython3.11.so'), python_executable('/opt/conda/envs/myenv/bin/python')]), py_run_simple_string(\\\"import numpy as np; a = np.array([[1,2],[3,4]]); b = np.array([[5,6],[7,8]]); print(f'Matrix product:\\\\n{a @ b}')\\\"), py_finalize, halt\""
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

🎉 Scryer-Python + Conda Demo: SUCCESS!
```

## Dockerfile Breakdown

1. **Base Image**: Ubuntu 22.04
2. **System Dependencies**: curl, git, build tools, pkg-config
3. **Miniconda**: Installed to `/opt/conda`
4. **Conda Environment**: `myenv` with Python 3.11, NumPy, requests
5. **Scryer Prolog**: Built from source
6. **Demo Script**: Shows conda + NumPy integration

## Key Points

- Uses `python_executable` option pointing to conda env's Python
- Python automatically finds packages in the conda environment
- No need for `python_home` when using `python_executable`
- Demonstrates scientific Python ecosystem integration

## Conda vs UV

**Conda advantages**:
- Includes pre-built scientific libraries (NumPy, SciPy, etc.)
- Manages non-Python dependencies
- Widely used in data science/ML

**UV advantages** (see `examples/docker/`):
- Much faster package installation
- Smaller image size
- Simpler setup

Choose based on your needs!

## Customizing

Add more conda packages:

```dockerfile
RUN conda create -n myenv python=3.11 -y && \
    conda run -n myenv conda install numpy pandas scipy matplotlib -y
```

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
