FROM ubuntu:22.04

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libssl-dev \
    pkg-config \
    ca-certificates \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Install Miniconda
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh && \
    bash /tmp/miniconda.sh -b -p /opt/conda && \
    rm /tmp/miniconda.sh

ENV PATH="/opt/conda/bin:$PATH"

# Create a conda environment with Python 3.11 and install packages
# Use --override-channels to avoid TOS requirements from default channels
RUN conda create -n myenv python=3.11 --override-channels -c conda-forge -y && \
    conda run -n myenv pip install requests numpy

# Install Rust and Scryer Prolog from source
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:$PATH"

WORKDIR /tmp
RUN git clone https://github.com/mthom/scryer-prolog.git && \
    cd scryer-prolog && \
    cargo build --release && \
    mv target/release/scryer-prolog /usr/local/bin/ && \
    cd .. && rm -rf scryer-prolog

# Create working directory
WORKDIR /app

# Copy library source
COPY src/ /app/src/

# Find the conda environment's Python version dynamically
RUN PYTHON_VERSION=$(conda run -n myenv python -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')") && \
    echo "PYTHON_VERSION=$PYTHON_VERSION" > /app/conda_env.sh

# Create a demo script
RUN cat > /app/demo.pl << 'EOF'
:- use_module(library(os)).
:- use_module(library(lists)).
:- use_module('src/lib/python').
:- initialization(main).

main :-
    % Find Python version from conda env
    getenv("PYTHON_VERSION", PyVerStr),
    atom_chars(PyVerAtom, PyVerStr),

    % Build paths
    atom_concat('/opt/conda/envs/myenv/lib/libpython', PyVerAtom, LibPrefix),
    atom_concat(LibPrefix, '.so', LibPath),

    py_initialize([
        shared_library_path(LibPath),
        python_executable('/opt/conda/envs/myenv/bin/python')
    ]),

    py_run_simple_string("import sys"),
    py_run_simple_string("print(f'Python version: {sys.version}')"),
    py_run_simple_string("print(f'Python prefix: {sys.prefix}')"),
    py_run_simple_string("print(f'Conda env: {sys.prefix}')"),
    py_run_simple_string("print()"),

    py_run_simple_string("import numpy as np"),
    py_run_simple_string("print(f'NumPy version: {np.__version__}')"),
    py_run_simple_string("arr = np.array([1, 2, 3, 4, 5])"),
    py_run_simple_string("print(f'NumPy array sum: {arr.sum()}')"),
    py_run_simple_string("print()"),

    py_run_simple_string("import requests"),
    py_run_simple_string("response = requests.get('https://api.github.com/repos/mthom/scryer-prolog')"),
    py_run_simple_string("print(f'Scryer Prolog GitHub stars: {response.json()[\"stargazers_count\"]}')"),
    py_run_simple_string("print()"),

    py_run_simple_string("print('🎉 Scryer-Python + Conda Demo: SUCCESS!')"),

    py_finalize,
    halt.
EOF

# Source conda env variables at runtime
CMD ["/bin/bash", "-c", "source /app/conda_env.sh && scryer-prolog demo.pl"]
