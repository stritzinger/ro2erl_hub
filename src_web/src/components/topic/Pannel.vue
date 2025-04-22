<script setup>
import Card from '../Card.vue'
import Bandwidth from './Bandwidth.vue'
import { computed, ref } from 'vue'
import { useDataStore } from '@/stores/data'
import { storeToRefs } from 'pinia'
import { breakpointsTailwind, useBreakpoints } from '@vueuse/core'

const { topicList } = storeToRefs(useDataStore())

const breakpoint = computed(() => useBreakpoints(breakpointsTailwind))
const lgUp = ref(breakpoint.value.greater('lg'))

defineProps({
  title: {
    type: String,
    default: '',
  },
})
</script>

<template>
  <Card :title="title">
    <div v-if="topicList.length > 0">
      <header
        v-if="lgUp"
        class="hub-topic-grid py-2 px-3 mt-2 bg-sky-100 dark:bg-neutral-800 font-bold"
      >
        <span class="text-left">Name</span>
        <span class="text-center">Dropped</span>
        <span class="text-center">Forwarded</span>
        <span class="text-center">Filterable</span>
        <span class="text-right col-span-2">Bandwidth Limit</span>
      </header>
      <ul class="hub-list hub-divide-y">
        <li v-for="topic in topicList" :key="topic.topic_name" class="py-4">
          <div v-if="lgUp" class="hub-topic-grid font-code text-sm">
            <span class="hub-topic-grid-item--left">
              {{ topic.topic_name }}</span
            >
            <span class="hub-topic-grid-item--center flex-col text-sm">
              <div>{{ topic.metrics.dropped.bandwidth }} B/s</div>
            </span>
            <span class="hub-topic-grid-item--center flex-col text-sm">
              <div>{{ topic.metrics.forwarded.bandwidth }} B/s</div>
              <div>
                <span>Rate: </span
                >{{ Number(topic.metrics.forwarded.rate).toFixed(2) }}
              </div>
            </span>
            <span class="hub-topic-grid-item--center">
              <FontAwesomeIcon
                v-if="topic.filterable"
                :icon="['fas', 'check']"
              />
              <span v-else>&mdash;</span>
            </span>
            <span class="hub-topic-grid-item--right lg:col-span-2">
              <Bandwidth
                v-if="topic.filterable"
                :value="topic.bandwidth_limit"
                :name="topic.topic_name"
              />
            </span>
          </div>
          <div v-else class="flex flex-col gap-1 font-code">
            <div class="flex flex-row gap-2">
              <strong class="font-meta">Name: </strong>
              <span>{{ topic.topic_name }}</span>
            </div>
            <div class="flex flex-col">
              <div class="flex flex-row gap-2 items-center">
                <strong class="font-meta">Dropped: </strong>
                <span class="flex flex-row gap-2 text-sm">
                  <div>{{ topic.metrics?.dropped?.bandwidth }} B/s</div>
                </span>
              </div>
              <div class="flex flex-row gap-2 items-center">
                <strong class="font-meta">Forwarded: </strong>
                <span class="flex flex-row gap-2 text-sm">
                  <div>{{ topic.metrics.forwarded.bandwidth }} B/s</div>
                  <div>
                    <span>Rate: </span
                    >{{ Number(topic.metrics.forwarded.rate).toFixed(2) }}
                  </div>
                </span>
              </div>
              <div class="flex flex-row gap-2 items-center">
                <strong class="font-meta">Filterable: </strong>
                <FontAwesomeIcon
                  v-if="topic.filterable"
                  :icon="['fas', 'check']"
                />
                <span v-else>&mdash;</span>
              </div>
            </div>

            <span class="hub-topic-grid-item--right lg:col-span-2">
              <Bandwidth
                v-if="topic.filterable"
                :value="topic.bandwidth_limit"
                :name="topic.topic_name"
              />
            </span>
          </div>
        </li>
      </ul>
    </div>
    <div v-else class="py-4"><p>No topics found</p></div>
  </Card>
</template>

<style scoped>
@reference "tailwindcss";

.hub-topic-grid {
  @apply grid grid-cols-1 lg:grid-cols-6 gap-2;
}
.hub-topic-grid-item--center {
  @apply flex lg:items-center lg:justify-center;
}
.hub-topic-grid-item--right {
  @apply flex lg:items-end lg:justify-center;
}
.hub-topic-grid-item--left {
  @apply flex lg:items-center lg:justify-start;
}
</style>
