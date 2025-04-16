<script setup>
import { computed } from 'vue'
import { invertedShadeMap } from '../utils/colors'

const props = defineProps({
  color: {
    type: String,
    default: 'grisp-50',
  },
  rounded: {
    type: String,
    default: null,
  },
})

const palette = computed(() => {
  const [colorBase, colorShade] = props.color.split('-')
  const shadeNumber = parseInt(colorShade)
  if (isNaN(shadeNumber)) return { base: colorBase, shade: 500 }
  return { base: colorBase, shade: shadeNumber }
})
</script>

<template>
  <div
    :class="[
      'alert-dialog py-2 px-4',
      'bg-' + palette.base + '-' + palette.shade,
      'text-' + palette.base + '-' + invertedShadeMap[palette.shade],
    ]"
  >
    <slot></slot>
  </div>
</template>

<style scoped>
@reference "tailwindcss";

.alert-dialog {
  @apply w-full;
}
</style>
