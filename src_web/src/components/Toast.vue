<template>
  <div class="fixed top-14 right-8 z-50 flex flex-col gap-2">
    <div
      v-for="toast in toasts"
      :key="toast.id"
      :class="[
        'p-4 rounded shadow  w-64',
        toast.type === 'success'
          ? 'bg-green-400 text-green-900'
          : toast.type === 'error'
            ? 'bg-red-600 text-white'
            : 'bg-gray-800 text-white',
      ]"
    >
      <span class="flex items-center gap-2">
        <FontAwesomeIcon :icon="['fas', 'exclamation-triangle']" />
        {{ toast.message }}
      </span>
    </div>
  </div>
</template>

<script setup>
import { useAppStore } from '@/stores/app'
import { storeToRefs } from 'pinia'
const { toasts } = storeToRefs(useAppStore())
</script>
