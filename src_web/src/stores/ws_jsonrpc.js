import { defineStore } from 'pinia'
import { ref } from 'vue'
import { v4 as uuidv4 } from 'uuid'

import { useAppStore } from './app'

const urls = {
  'ro2erl-hub': `${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}/ws`,
}

export const useWsStore = defineStore('ws', () => {
  const appStore = useAppStore()

  // State
  const connections = ref(new Map())
  const pendingRequests = ref(new Map())
  const onCloseCounters = ref(new Map())
  const notificationHandlers = ref(new Map())

  // --- Connection Management -------------------------------------------------
  function connect(name, token) {
    return new Promise((resolve) => {
      if (connections.value.has(name)) {
        resolve(connections.value.get(name))
        return
      }
      const url = token ? `${urls[name]}?token=${token}` : urls[name]
      const ws = new WebSocket(url, 'jsonrpc-2.0')
      ws.onmessage = (e) => onMessage(e.data, name)
      ws.onopen = () => {
        onOpened(name)
        resolve(ws)
      }
      ws.onclose = (e) => {
        console.log(`WS Closed: ${name}`)
        onClosed(e, name)
        resolve(null)
      }
      connections.value.set(name, ws)
      onCloseCounters.value.set(name, 0)
    })
  }

  function onOpened(name) {
    console.log(`WS Opened: ${name}`)
  }

  function onClosed(e, name) {
    connections.value.delete(name)
    let count = (onCloseCounters.value.get(name) || 0) + 1
    onCloseCounters.value.set(name, count)
    if (e.code === 1000 && count < 5) {
      connect(name) // Auto-reconnect logic
    } else {
      cleanupConnection(name)
    }
  }

  function cleanupConnection(name) {
    connections.value.delete(name)
    onCloseCounters.value.delete(name)
    notificationHandlers.value.delete(name)
    // Remove all pending requests for this connection
    for (const [id, req] of pendingRequests.value.entries()) {
      req.reject?.({
        error: 'websocket_closed',
        type: 'internal',
        connection: name,
      })
      pendingRequests.value.delete(id)
    }
  }

  function isOpen(name) {
    return connections.value.get(name)?.readyState === 1
  }

  // --- Messaging -------------------------------------------------------------
  function buildMessage(method, params) {
    return {
      jsonrpc: '2.0',
      id: uuidv4(),
      method,
      params,
    }
  }

  function send(name, message) {
    const ws = connections.value.get(name)
    if (!ws || ws.readyState !== 1) {
      return Promise.reject({
        error: 'websocket_down',
        type: 'internal',
      })
    }
    return new Promise((resolve, reject) => {
      pendingRequests.value.set(message.id, { resolve, reject })
      ws.send(JSON.stringify(message))
    }).catch((data) => {
      appStore.showToast(data.error.message, 'error')
    })
  }

  function onMessage(json, name) {
    const message = JSON.parse(json)
    if (message.id) {
      onResponse(message)
    } else {
      onNotification(message, name)
    }
  }

  function onResponse(response) {
    const request = pendingRequests.value.get(response.id)
    pendingRequests.value.delete(response.id)
    if (request) {
      if (response.result) {
        request.resolve(response)
      } else {
        response.type = 'response'
        request.reject(response)
      }
    }
  }

  // --- Notification Handlers -------------------------------------------------
  function registerNotificationHandler(connectionName, handler) {
    notificationHandlers.value.set(connectionName, handler)
  }

  function unregisterNotificationHandler(connectionName) {
    notificationHandlers.value.delete(connectionName)
  }

  function onNotification(notification, name) {
    const handler = notificationHandlers.value.get(name)
    if (handler) {
      try {
        handler(notification)
      } catch (error) {
        console.error(`Error in notification handler for ${name}:`, error)
      }
    }
  }

  // --- Expose API ------------------------------------------------------------
  return {
    connect,
    send,
    isOpen,
    buildMessage,
    registerNotificationHandler,
    unregisterNotificationHandler,
    cleanupConnection, // Expose for manual cleanup if needed
  }
})
