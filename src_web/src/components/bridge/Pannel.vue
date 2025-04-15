<script setup>
import Card from '../Card.vue'
import { useDataStore } from '@/stores/data'

const { bridgeList } = useDataStore()

defineProps({
  title: {
    type: String,
    default: '',
  },
})
</script>

<template>
  <Card :title="title">
    <ul class="hub-list hub-divide-y font-code">
      <li
        v-for="bridge in bridgeList"
        :key="bridge.bridge_id"
        class="py-4 px-3"
      >
        <span>{{ bridge.bridge_id }}</span>
      </li>
    </ul>
  </Card>
</template>

<style scoped>
@reference "tailwindcss";
</style>
