import { ref, computed } from 'vue'
import { defineStore } from 'pinia'

export const useDataStore = defineStore('data', () => {
  const methods = {
    bridge: {
      list: "bridge.list",
      attached: "bridge.attached",
      detached: "bridge.detached",
    },
    topic: {
      list: "topic.list",
      updated: "topic.updated",
      setBandwidth: "topic.setBandwidth",
    }
  }
  const topic_list = ref([
    {
      "topic_name": "<topic_name_1>",
      "filterable": true,
      "bandwidth_limit": 1024,
      "metrics": {
        "dispatched": {
          "bandwidth": 512,
          "rate": 2.5
        },
        "forwarded": {
          "bandwidth": 768,
          "rate": 3.2
        }
      }
    },
    {
      "topic_name": "<topic_name_2>",
      "filterable": false,
      "bandwidth_limit": null,
      "metrics": {
        "dispatched": {
          "bandwidth": 128,
          "rate": 0.5
        },
        "forwarded": {
          "bandwidth": 128,
          "rate": 0.5
        }
      }
    }
  ])
  const bridge_list = ref([
    {"bridge_id": "bridge1"},
    {"bridge_id": "bridge2"}
  ])

  const bridgeList = computed(() => {
    return bridge_list.value
  })

  const topicList = computed(() => {
    return topic_list.value
  })
  
  return { bridgeList, topicList }
})
