import { ref, computed } from 'vue'
import { defineStore } from 'pinia'

export const useDataStore = defineStore('data', () => {
  const topic_list = ref([])
  const bridge_list = ref([])

  const bridgeList = computed(() => {
    return bridge_list.value
  })

  const topicList = computed(() => {
    return topic_list.value
  })

  function setBridgeList(list) {
    bridge_list.value = [...list]
  }

  function setTopicList(list) {
    topic_list.value = [...list]
  }

  function getDispatchedStats() {
    return Number(
      topic_list.value
        .reduce((acc, topic) => {
          acc += topic.metrics.dispatched.bandwidth
          return acc
        }, 0)
        .toFixed(2),
    )
  }

  function getForwardedStats() {
    return Number(
      topic_list.value
        .reduce((acc, topic) => {
          acc += topic.metrics.forwarded.bandwidth
          return acc
        }, 0)
        .toFixed(2),
    )
  }

  return {
    bridgeList,
    topicList,
    setBridgeList,
    setTopicList,
    getDispatchedStats,
    getForwardedStats,
  }
})
