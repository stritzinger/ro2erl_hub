<script setup>
defineProps({
  title: {
    type: String,
    default: '',
  },
})
</script>

<template>
  <div class="hub-card">
    <div class="py-1.5 hub-card-header">
      <h1 class="text-2xl font-bold">{{ title }}</h1>
    </div>
    <slot></slot>
  </div>
</template>

<style scoped>
@reference "tailwindcss";

.hub-card {
  @apply flex flex-col;
}

.hub-card-header {
  @apply border-b-2 border-neutral-800 text-neutral-800 dark:border-neutral-300 dark:text-neutral-300;
}
</style>
