FROM jetpackio/devbox:latest

# Installing your devbox project
WORKDIR /code
USER root:root
RUN mkdir -p /code && chown ${DEVBOX_USER}:${DEVBOX_USER} /code
USER ${DEVBOX_USER}:${DEVBOX_USER}
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} devbox.json devbox.json
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} devbox.lock devbox.lock


# # Copying local flakes directories
# COPY my-flakes/sbt-overlay-flakes my-flakes/sbt-overlay-flakes
# COPY my-flakes/riscv-toolchain-flakes my-flakes/riscv-toolchain-flakes
# COPY my-flakes/riscv-toolchain-flakes my-flakes/riscv-toolchain-flakes

RUN git submodule update --init --recursive

RUN devbox run -- echo "Installed Packages."

RUN devbox shellenv --init-hook >> ~/.profile
