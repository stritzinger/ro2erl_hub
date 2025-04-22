import { ref, computed } from 'vue'
import { defineStore } from 'pinia'

export const useDataStore = defineStore('data', () => {
  const topic_list = ref([])
  const bridge_list = ref([])

  const bridgeList = computed(() => {
    return bridge_list.value
  })

  const topicList = computed(() => {
    return topic_list.value.map((topic) => ({
      ...topic,
      metrics: {
        ...topic.metrics,
        dropped: {
          bandwidth: Number(
            (
              topic.metrics.dispatched.bandwidth -
              topic.metrics.forwarded.bandwidth
            ).toFixed(2),
          ),
        },
      },
    }))
  })

  function setBridgeList(list) {
    bridge_list.value = [...list]
  }

  function setTopicList(list) {
    topic_list.value = [...list]
  }

  function getDroppedStats() {
    return Number(
      topic_list.value
        .reduce((acc, topic) => {
          acc += topic.metrics?.dropped?.bandwidth || 0
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
    getDroppedStats,
    getForwardedStats,
  }
})
