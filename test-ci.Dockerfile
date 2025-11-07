FROM ubuntu:22.04

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Install system dependencies for Python
RUN apt-get update && apt-get install -y \
    software-properties-common \
    git \
    build-essential \
    libssl-dev \
    pkg-config \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Add deadsnakes PPA for Python 3.11
RUN add-apt-repository ppa:deadsnakes/ppa -y && \
    apt-get update

# Install Python 3.11 with dev libraries
RUN apt-get install -y \
    python3.11 \
    python3.11-dev \
    libpython3.11 \
    libpython3.11-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:$PATH"

# Clone and build Scryer Prolog fork
WORKDIR /tmp
RUN git clone https://github.com/jjtolton/scryer-prolog.git && \
    cd scryer-prolog && \
    git checkout rtld-global-support && \
    cargo build --release && \
    mv target/release/scryer-prolog /usr/local/bin/ && \
    cd .. && rm -rf scryer-prolog

# Verify Scryer Prolog installation
RUN scryer-prolog -v

# Set up working directory
WORKDIR /scrypy

# Copy source code
COPY . .

# Run smoke tests
RUN echo "=== Running smoke tests ===" && \
    scryer-prolog examples/python_demo.pl && \
    echo "✓ python_demo.pl passed" && \
    scryer-prolog examples/python_demo_v2.pl && \
    echo "✓ python_demo_v2.pl passed"

# Run test suite
RUN echo "=== Running test suite ===" && \
    scryer-prolog examples/tests/test_all_types.pl && \
    echo "✓ test_all_types.pl passed" && \
    scryer-prolog examples/tests/test_dict.pl && \
    echo "✓ test_dict.pl passed" && \
    scryer-prolog examples/tests/test_dict_to_list.pl && \
    echo "✓ test_dict_to_list.pl passed" && \
    scryer-prolog examples/tests/test_globals_locals.pl && \
    echo "✓ test_globals_locals.pl passed" && \
    scryer-prolog examples/tests/test_memory_management.pl && \
    echo "✓ test_memory_management.pl passed"

RUN echo "=== ALL CI TESTS PASSED ==="

CMD ["bash"]
