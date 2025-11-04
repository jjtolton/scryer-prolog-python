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
    && rm -rf /var/lib/apt/lists/*

# Install uv (fast Python package manager)
RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:$PATH"

# Install Python 3.11.14 with uv
RUN uv python install 3.11.14

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

# Create a virtual environment and install Python packages
RUN uv venv && \
    uv pip install requests

# Create a demo script
RUN cat > /app/demo.pl << 'EOF'
:- use_module(library(os)).
:- use_module('src/lib/python').
:- initialization(main).

main :-
    getenv("HOME", HomeDirStr),
    atom_chars('/.local/share/uv/python/cpython-3.11.14-linux-x86_64-gnu/lib/libpython3.11.so', LibSuffix),
    append(HomeDirStr, LibSuffix, LibPathStr),
    atom_chars(LibPath, LibPathStr),

    py_initialize([
        shared_library_path(LibPath),
        python_executable('/app/.venv/bin/python3')
    ]),

    py_run_simple_string("import sys"),
    py_run_simple_string("print(f'Python version: {sys.version}')"),
    py_run_simple_string("print(f'Python prefix: {sys.prefix}')"),
    py_run_simple_string("print()"),

    py_run_simple_string("import requests"),
    py_run_simple_string("print(f'Requests version: {requests.__version__}')"),
    py_run_simple_string("print()"),

    py_run_simple_string("response = requests.get('https://api.github.com/repos/mthom/scryer-prolog')"),
    py_run_simple_string("print(f'Scryer Prolog GitHub stars: {response.json()[\"stargazers_count\"]}')"),
    py_run_simple_string("print()"),
    py_run_simple_string("print('🎉 Scryer-Python Docker Demo: SUCCESS!')"),

    py_finalize,
    halt.
EOF

# Default command
CMD ["scryer-prolog", "demo.pl"]
