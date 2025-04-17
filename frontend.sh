#!/usr/bin/env sh
set -eu

# Colors:
RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'
YELLOW='\033[0;33m'


# Checks that a valid NodeJS version is installed.
check_nodejs() {
    README_MSG="${YELLOW}Check README.md to see how to install NodeJS.${RESET}"

    # Check for Node.js executable
    if ! command -v node --version >/dev/null 2>&1; then
        printf "${RED}Error:${RESET} Node.js is not available!\n"
        printf "%b\n" "$README_MSG"
        exit 1
    # Check Node.js version
    elif [ $(node --version | cut -c2-3) -lt 22 ]; then
        printf "${RED}Error:${RESET} Your Node.js version is too old!\n"
        printf "%b\n" "$README_MSG"
        exit 1
    fi
}

# These variables are initialized for the development build:
include_dev_deps=""   # Skip or install devDependencies
log_level="warn"  # Log level for `npm install`
build_script="dev" # Which script to run from `package.json`

# Set the above variables based on the value of $NPM_BUILD_PROFILE.
# $NPM_BUILD_PROFILE is either unset, or set by shell_hooks_env in the rebar.config.
# If it's unset, we'll treat it as the development build profile.
set_build_config() {
    if [ "${NPM_BUILD_PROFILE:-}" = "prod" ]; then
        # Build for production (in the CI).
        # We want to skip devDependencies, and we want lots of logs.
        # Note: dev deps are automatically skipped because NODE_ENV will be "production".
        build_script="prod"
        log_level="notice"
    elif [ "${NPM_BUILD_PROFILE:-}" = "playwright" ]; then
        # Build for running Playwright tests.
        # We build for production, but we'll also install devDependencies.
        build_script="prod"
        include_dev_deps="--include=dev"
    else
        # Build for development. The values are already correct.
        # Note: The colon character is a "no-op" instruction: "do nothing".
        :
    fi
}

check_nodejs
set_build_config

printf "${GREEN}Building the frontend (mode $build_script)...${RESET}\n"

cd src_web
FORCE_COLOR=true NPM_CONFIG_FUND=false npm install --logLevel=$log_level --audit=false $include_dev_deps
FORCE_COLOR=true npm run $build_script

printf "${GREEN}Frontend built.${RESET}\n"
