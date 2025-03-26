# RO2ERL Hub

A central message router for the Target-X system, managing communication between distributed ROS2/DDS networks through ro2erl bridges.

## Overview

ro2erl_hub is a central component in the Target-X system that facilitates message routing between distributed ROS2 networks. It works in conjunction with ro2erl_bridge components to enable secure and reliable communication between ROS2 nodes across different sites.

For detailed design information, see:
- [Hub Design Documentation](doc/design.md)
- [RO2ERL Documentation](https://github.com/stritzinger/ro2erl_doc)

## Requirements

- Erlang/OTP 27 or later
- rebar3

## Development

For local development and testing:

```bash
# Start an interactive shell with debug logging and a specific node name
rebar3 shell --sname hub@localhost --setcookie targetx

# Run tests
rebar3 ct
```

The hub node will be started with the short name `hub` and the cookie `targetx`. This allows other nodes (like bridges) to connect to it during development.

## Production Deployment

For production deployment on grisp.io:

```bash
# Build the release with production dependencies
rebar3 as prod release

# Build the Docker image for deployment
rebar3 as prod docker

# Push the image to your registry
docker push local/ro2erl_hub:0.1.0 your-registry/ro2erl_hub:0.1.0
```

The production build includes the braidnode dependency which is required for deployment on grisp.io. This dependency is not included in the development environment to simplify local testing.

## License

[License information to be added] 
