FROM debian:bullseye-slim

ENV LANG C.UTF-8

# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="true"

# Install needed packages and setup non-root user.
# Use a separate RUN statement to add your own dependencies
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
COPY library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && \
    export DEBIAN_FRONTEND=noninteractive && \
    /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true" && \
    rm -rf /tmp/library-scripts && \
    apt-get install -y --no-install-recommends \
        dpkg-dev \
        gcc \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        libnuma-dev \
        libtinfo-dev \
        zlib1g-dev && \
    apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Install latest GHCup in the non-root user home
USER $USERNAME
RUN mkdir -p "$HOME/.ghcup/bin" && \
    ARCH="$(uname -m)" && \
    curl -LJ "https://downloads.haskell.org/~ghcup/$ARCH-linux-ghcup" -o "$HOME/.ghcup/bin/ghcup" && \
    chmod +x "$HOME/.ghcup/bin/ghcup"
ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"

# Install recommened (on 2023/09/12) versions of GHC and Cabal
RUN ghcup install ghc "9.2.8" --set && \
    ghcup install cabal "3.6.2.0" --set && \
    cabal update

# Move Stack configuration files to its root
COPY --chown=vscode:vscode --chmod=777 "./.stack" "/home/$USERNAME/.stack"

# Install Stack and install utility libraries
RUN ARCH="$(uname -m)" && \
    if [ "$ARCH"=="x86_64" ]; then \
        ghcup install stack "2.9.3"; \
    else \
        mkdir -p "$HOME/.local/bin" && cd "$HOME/.local/bin" && \
        curl -LJ "https://downloads.haskell.org/ghcup/unofficial-bindists/stack/2.9.3/stack-2.9.3-linux-aarch64.tar.gz" -o "./stack.tar.gz" && \
        tar -xzf "./stack.tar.gz" && \
        rm "./stack.tar.gz" && \
        chmod +x "./stack"; \
    fi && \
    stack install implicit-hie && \
    stack install hspec-discover
