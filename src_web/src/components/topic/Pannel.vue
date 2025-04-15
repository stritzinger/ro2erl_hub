<script setup>
import Card from '../Card.vue'
import Bandwidth from './Bandwidth.vue'
import { useDataStore } from '@/stores/data'

const { topicList } = useDataStore()

defineProps({
  title: {
    type: String,
    default: '',
  },
})
</script>

<template>
  <Card :title="title">
    <header
      class="hub-topic-grid py-2 px-3 mt-2 bg-neutral-100 dark:bg-neutral-800"
    >
      <span class="text-left">Name</span>
      <span class="text-center">Filterable</span>
      <span class="text-center">Bandwidth Limit</span>
      <span class="text-center">Dispatched</span>
      <span class="text-right">Forwarded</span>
    </header>
    <ul class="hub-list hub-divide-y">
      <li
        v-for="topic in topicList"
        :key="topic.topic_name"
        class="py-4 hub-topic-grid font-code px-3"
      >
        <span class="hub-topic-grid-item--left">{{ topic.topic_name }}</span>
        <span class="hub-topic-grid-item--center">
          <FontAwesomeIcon v-if="topic.filterable" :icon="['fas', 'check']" />
          <FontAwesomeIcon v-else :icon="['fas', 'plus']" class="rotate-45" />
        </span>
        <span class="hub-topic-grid-item--center">
          <Bandwidth :value="topic.bandwidth_limit" :name="topic.topic_name" />
        </span>
        <span class="hub-topic-grid-item--center flex-col text-sm">
          <div>{{ topic.metrics.dispatched.bandwidth }} MB/s</div>
          <div><span>Rate: </span>{{ topic.metrics.dispatched.rate }}</div>
        </span>
        <span class="hub-topic-grid-item--right flex-col text-sm">
          <div>{{ topic.metrics.forwarded.bandwidth }} MB/s</div>
          <div><span>Rate: </span>{{ topic.metrics.forwarded.rate }}</div>
        </span>
      </li>
    </ul>
  </Card>
</template>

<style scoped>
@reference "tailwindcss";

.hub-topic-grid {
  @apply grid grid-cols-5 gap-2;
}
.hub-topic-grid-item--center {
  @apply flex items-center justify-center;
}
.hub-topic-grid-item--right {
  @apply flex items-end justify-center;
}
.hub-topic-grid-item--left {
  @apply flex items-center justify-start;
}
</style>
