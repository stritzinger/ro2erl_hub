<script setup>
import { ref } from 'vue'
import { useRo2erlHubStore } from '@/stores/ro2erlHub'

const { setBandwidth, clearBandwidth } = useRo2erlHubStore()
const props = defineProps({
  value: {
    type: Number,
    default: null,
  },
  name: {
    type: String,
    required: true,
  },
})

const inputValue = ref(props.value)

const handleSubmit = async () => {
  await setBandwidth(props.name, inputValue.value)
  resetInput()
}

const handleClear = async () => {
  await clearBandwidth(props.name)
  resetInput()
}

const handleInput = (event) => {
  // Convert to number and ensure it's positive
  const value = Math.max(0, Number(event.target.value) || 0)
  inputValue.value = value || null // Set to null if value is 0
}

const resetInput = () => {
  inputValue.value = props.value
}
</script>

<template>
  <form @submit.prevent="handleSubmit" class="bandwidth-control">
    <div class="bandwidth-control-input">
      <p
        v-if="value === null"
        class="text-sm font-code dark:text-white pb-2 mb-2 border-b border-neutral-400 dark:border-neutral-600"
      >
        Unlimited
      </p>
      <label
        for="bandwidth-input"
        class="text-xs font-meta font-bold text-blue-800 dark:text-white"
        >Bandwidth Limit (B/s)</label
      >
      <input
        type="number"
        :value="inputValue"
        @input="handleInput"
        class="bandwidth-input"
        placeholder="e.g. 560"
      />
    </div>
    <div class="bandwidth-control-buttons">
      <button
        type="submit"
        class="bandwidth-button bg-grisp-700 border border-grisp-700 hover:bg-grisp-400 hover:border-grisp-400 dark:bg-grisp-500 dark:border-grisp-500 dark:hover:bg-grisp-400 dark:hover:border-grisp-400"
        :disabled="inputValue === value"
      >
        <span class="flex gap-1 items-center">
          <FontAwesomeIcon :icon="['fas', 'check']" />
          Set
        </span>
      </button>
      <div v-if="value !== null" class="divider"></div>
      <button
        v-if="value !== null"
        class="bandwidth-button--link text-grisp-700 bg-grisp-50/20 border border-grisp-700 hover:bg-grisp-50/60 hover:border-grisp-400 dark:text-grisp-500 dark:bg-grisp-500/20 dark:border-grisp-500 dark:hover:bg-grisp-500/30 dark:hover:border-grisp-400"
        @click.prevent="handleClear"
        :disabled="inputValue === null"
      >
        <span class="flex gap-1 items-center">
          <FontAwesomeIcon :icon="['fas', 'xmark']" />
        </span>
      </button>
    </div>
  </form>
</template>

<style scoped>
@reference "tailwindcss";

.bandwidth-control {
  @apply flex gap-1 items-end w-full;
}

.bandwidth-control-input {
  @apply flex flex-col w-full grow;
}

.bandwidth-control-buttons {
  @apply flex gap-1 items-center;
}

.divider {
  @apply h-4 w-px bg-neutral-400 dark:bg-neutral-600;
}

.bandwidth-input {
  @apply px-2 py-1 w-24 text-sm border grow w-full rounded
    bg-white dark:bg-neutral-700
    border-blue-800 dark:border-white
    focus:border-blue-500 dark:focus:border-blue-200
    focus:ring-1 focus:ring-blue-500 dark:focus:ring-blue-200
    outline-none;

  /* Hide spinner buttons in all browsers */
  /* Chrome, Safari, Edge, Opera */
  &::-webkit-outer-spin-button,
  &::-webkit-inner-spin-button {
    -webkit-appearance: none;
    margin: 0;
  }

  /* Firefox */
  &[type='number'] {
    -moz-appearance: textfield;
  }
}

.bandwidth-button {
  @apply px-3 py-1.5 text-xs rounded flex-nowrap
    text-white
    disabled:opacity-50 disabled:cursor-not-allowed
    transition-colors duration-200 cursor-pointer;
}

.bandwidth-button--link {
  @apply px-3 py-2 text-xs rounded 
    px-2 border disabled:opacity-50
    disabled:cursor-not-allowed cursor-pointer 
    transition-colors duration-200;
}
</style>
