import { defineStore } from 'pinia'
import { useWsStore } from './ws_jsonrpc'
import { useDataStore } from './data'
import { useAppStore } from './app'
import { ref } from 'vue'
const name = 'ro2erl-hub'

const methods = {
  bridge: {
    list: 'bridge.list',
    attached: 'bridge.attached',
    detached: 'bridge.detached',
  },
  topic: {
    list: 'topic.list',
    updated: 'topic.updated',
    setBandwidth: 'topic.setBandwidth',
  },
}

export const useRo2erlHubStore = defineStore('ro2erl-hub', () => {
  /**
   * This store is responsible for fetching data from the ro2erl-hub API.
   *
   */

  const isOpen = ref(null)

  const wsStore = useWsStore()
  const { setBridgeList, setTopicList } = useDataStore()
  const { showToast } = useAppStore()
  async function connect(token) {
    const ws = await wsStore.connect(name, token)
    if (!ws) isOpen.value = false
  }

  async function initStateAndWS(token) {
    await connect(token)
    await getBridgeList()
    await getTopicList()
    await registerNotificationHandlers()
    isOpen.value = wsStore.isOpen(name)
  }

  async function registerNotificationHandlers() {
    wsStore.registerNotificationHandler(name, onUpdateNotification)
  }

  async function onUpdateNotification(notification) {
    switch (notification.method) {
      case methods.topic.updated:
        getTopicList()
        break
      case methods.bridge.attached:
        getBridgeList()
        break
      case methods.bridge.detached:
        getBridgeList()
        break
      default:
        console.log('Unknown notification:', notification)
    }
  }

  async function getBridgeList() {
    const message = wsStore.buildMessage(methods.bridge.list, {})
    const data = await wsStore.send(name, message)
    if (data) setBridgeList(data.result)
    return data
  }

  async function getTopicList() {
    const message = wsStore.buildMessage(methods.topic.list, {})
    const data = await wsStore.send(name, message)
    if (data) setTopicList(data.result)
    return data
  }

  async function setBandwidth(topic_name, bandwidth) {
    const message = wsStore.buildMessage(methods.topic.setBandwidth, {
      topic_name,
      bandwidth,
    })
    const data = await wsStore.send(name, message)
    if (data) showToast('Bandwidth set successfully', 'success')
    return data
  }

  async function clearBandwidth(topic_name) {
    return await setBandwidth(topic_name, null)
  }

  return {
    initStateAndWS,
    getBridgeList,
    getTopicList,
    setBandwidth,
    clearBandwidth,
    isOpen,
  }
})
