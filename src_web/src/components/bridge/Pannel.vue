<script setup>
import Card from '../Card.vue'
import { useDataStore } from '@/stores/data'
import { storeToRefs } from 'pinia'

const { bridgeList } = storeToRefs(useDataStore())

defineProps({
  title: {
    type: String,
    default: '',
  },
})
</script>

<template>
  <Card :title="title">
    <div v-if="bridgeList.length > 0">
      <ul class="hub-list hub-divide-y font-code">
        <li v-for="bridge in bridgeList" :key="bridge" class="py-4 px-3">
          <span class="flex gap-2 items-center">
            <div class="relative flex w-[10px] h-[10px]">
              <FontAwesomeIcon
                :icon="['fas', 'circle']"
                size="2xs"
                class="absolute animate-ping inline-flex text-green-500"
              />
              <FontAwesomeIcon
                :icon="['fas', 'circle']"
                size="2xs"
                class="text-green-500"
              />
            </div>
            {{ bridge }}
          </span>
        </li>
      </ul>
    </div>
    <div v-else class="py-4">
      <p>No connected bridges found</p>
    </div>
  </Card>
</template>

<style scoped>
@reference "tailwindcss";
</style>
