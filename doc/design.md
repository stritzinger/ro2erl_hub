# RO2ERL Hub Design Document

## Overview

The `ro2erl_hub` component is a central element of the Target-X system, designed to route messages between distributed ROS2/DDS networks through ro2erl bridges. It operates as a central message router, managing connections from multiple bridges and ensuring reliable message distribution across the system.

## Architecture

### Role in Target-X System

The `ro2erl_hub` serves as a central message router in the Target-X system, operating at the core of the distributed network. It works in conjunction with multiple `ro2erl_bridge` components to provide:

- Centralized message routing between distributed ROS2 networks
- Bridge connection management and monitoring
- Message distribution to all connected bridges
- Automatic cleanup of disconnected bridges

### Component Structure

The hub is implemented as an Erlang application with the following key components:

#### Core Components

1. **Hub Server (`ro2erl_hub_server`):**
   - Main process handling bridge registration and message distribution
   - Manages connections from multiple bridges
   - Registers with process groups for discovery
   - Processes and forwards messages to all connected bridges

2. **Bridge Interface (`ro2erl_hub_bridge`):**
   - Provides abstraction layer for bridge communication
   - Defines protocol for hub-bridge interaction
   - Handles message formatting and delivery

3. **Hub Supervisor (`ro2erl_hub_sup`):**
   - Top-level supervisor ensuring fault tolerance
   - Manages all hub processes
   - Handles process group registration

### Operation Modes

The hub operates in two states:
- **Idle:** Initial state when no bridges are connected
- **Forwarding:** Active state when at least one bridge is connected

The hub automatically transitions between these states based on bridge connections and provides the following behavior:
- Accepts bridge connections
- Maintains list of connected bridges
- Forwards messages to all connected bridges
- Handles bridge disconnections and cleanup

### Data Structure

The hub maintains bridge and topic information in a cohesive structure:
- Bridge records include the bridge's topics as part of their state
- Topic information is consolidated on-demand when queried
- This design simplifies bridge management and ensures topic data is automatically cleaned up when bridges disconnect

## Communication Protocol

### Hub-Bridge Communication

All communication between the hub and bridges occurs through Erlang distribution, leveraging:

- **Process Groups:** Hub process registers in a process group that bridges monitor
- **Message Format:** Messages include metadata such as:
  - Bridge ID (unique identifier for the bridge instance)
  - Timestamp (when the message was sent)
  - Payload (the actual message content)

### Connection Management

The hub implements a higher-level protocol for bridge attachment and detachment:

1. **Bridge Attachment:**
   - Bridge sends attach message with its ID
   - Hub stores bridge information and sets up monitoring
   - Hub acknowledges attachment

2. **Bridge Detachment:**
   - Bridge sends detach message
   - Hub removes bridge from its list
   - Hub cleans up monitoring resources

3. **Automatic Cleanup:**
   - Hub monitors all connected bridges
   - Automatically removes crashed bridges
   - Logs bridge state changes

4. **Topic Management:**
   - Bridge sends topic updates to the hub with current metrics
   - Hub can adjust bandwidth limits for specific topics on bridges
   - Hub maintains a global view of all topics across all bridges

### Bridge Discovery

The hub utilizes Erlang's process group (`pg`) module for discovery:

1. Hub process registers in a specific process group
2. Bridges monitor this group to discover the hub
3. Bridges can attach to the hub when discovered
4. Hub maintains list of all attached bridges

### Hub-Bridge API

The communication between Hub and Bridge components follows the protocol defined in the main Target-X design document. For detailed message formats and protocol specifications, refer to:
[Target-X Design Document](https://github.com/stritzinger/ro2erl_doc/blob/main/design.md)

## Implementation Details

### State Management
- Uses `gen_statem` behavior for state transitions
- Maintains a map of connected bridges with their metadata
- Handles bridge monitoring and cleanup
- Tracks topic information from all bridges

### Message Processing
- Forwards messages to all connected bridges except the sender
- Maintains message metadata (bridge ID, timestamp) as received from bridges
- Implements message deduplication based on bridge IDs
- Processes topic information updates from bridges
- Manages topic bandwidth limits across all bridges

### Error Handling
- Graceful handling of bridge crashes
- Automatic cleanup of stale connections
- Logging of significant state changes

## Message Flow

### Message Distribution

1. Bridge sends message to hub
2. Hub receives message with bridge metadata
3. Hub forwards message to all other connected bridges
4. Each bridge processes the message locally

### Bridge State Changes

1. Bridge connects:
   - Sends attach message
   - Hub stores bridge information
   - Hub sets up monitoring
   - Hub logs connection

2. Bridge disconnects:
   - Sends detach message
   - Hub removes bridge information
   - Hub cleans up monitoring
   - Hub logs disconnection

3. Bridge crashes:
   - Hub detects crash through monitoring
   - Hub removes bridge information
   - Hub cleans up monitoring
   - Hub logs crash

### Topic Management Flow

1. Bridge collects topic metrics:
   - Tracks bandwidth usage per topic
   - Records message rates for dispatched and forwarded messages
   - Maintains information about filterable status and limits

2. Bridge reports topics to hub:
   - Periodically sends topic information to all connected hubs
   - Includes filterable status, bandwidth limits, and metrics
   - Hub stores the information with the bridge record

3. Hub consolidates topic data:
   - When topic information is requested, data is consolidated on-demand
   - For all topics view, all unique topics across bridges are identified efficiently
   - For single topic view, only bridges with that topic are processed

4. Hub manages bandwidth limits:
   - Analyzes traffic patterns across all bridges
   - Identifies congestion or overused topics
   - Sends bandwidth limit updates to bridges as needed
   - Bridges apply the new limits for traffic shaping

### Metrics Aggregation
- Bandwidth metrics (bytes/second) are summed across all bridges
- Message rates (messages/second) are summed across all bridges
- Topic filterability is determined conservatively (non-filterable if any bridge marks it so)
- Bandwidth limits use the most restrictive (minimum) value across bridges

## Configuration

### Required Configuration
- **Process Group Scope:** Name of the process group for discovery
- **Logging Level:** Configured separately for development and production

### Production Configuration
- **Braidnode Settings:** Required for deployment on grisp.io
- **Logging Level:** Set to notice for production

## Security Considerations

### Erlang Distribution Security

The hub relies on the security of the Erlang distribution for communication:

- When used with grisp.io, all communication is encrypted with TLS
- Certificate-based authentication through the grisp.io framework
- The hub itself does not implement security measures, delegating to the underlying platform

### Future Security Enhancements

- Message validation and authentication
- Rate limiting to prevent DoS attacks
- Enhanced logging and auditing

## Development Status

### Implemented Features
- Basic message routing
- Bridge connection management
- Process group registration
- Bridge monitoring

### Planned Features
- Message filtering based on rules
- Advanced metrics collection
- Traffic shaping and prioritization
- Enhanced error handling
- Centralized topic management and bandwidth control

## Deployment Considerations

### Supported Platforms
- **Linux Alpin Distributions:** Primary platform for standard deployments

### Integration Requirements
- Add `ro2erl_hub` as a dependency
- Ensure proper logging configuration
- For production, configure braidnode settings
