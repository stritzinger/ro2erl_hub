import { ref, computed } from 'vue'
import { defineStore } from 'pinia'

export const useDataStore = defineStore('data', () => {
  const topic_list = ref([
    {
      topic_name: '/sensor/data',
      filterable: true,
      bandwidth_limit: 1024,
      metrics: {
        dispatched: { bandwidth: 512, rate: 2.5 },
        forwarded: { bandwidth: 768, rate: 3.2 },
      },
    },
  ])
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
    return topic_list.value.reduce((acc, topic) => {
      acc += topic.metrics.dispatched.bandwidth
      return acc
    }, 0)
  }

  function getForwardedStats() {
    return topic_list.value.reduce((acc, topic) => {
      acc += topic.metrics.forwarded.bandwidth
      return acc
    }, 0)
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
