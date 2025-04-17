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
    <div class="py-2 border-b border-neutral-200">
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
</style>
