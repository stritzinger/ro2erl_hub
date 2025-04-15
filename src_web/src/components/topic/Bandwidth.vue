<script setup>
import { ref } from 'vue'

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

const handleSubmit = () => {
  console.log(inputValue.value)
}

const handleInput = (event) => {
  // Convert to number and ensure it's positive
  const value = Math.max(0, Number(event.target.value) || 0)
  inputValue.value = value || null // Set to null if value is 0
}
</script>

<template>
  <form @submit.prevent="handleSubmit" class="bandwidth-control">
    <input
      type="number"
      :value="inputValue"
      @input="handleInput"
      class="bandwidth-input"
      :placeholder="value === null ? 'Unlimited' : ''"
    />
    <button
      type="submit"
      class="bandwidth-button"
      :disabled="inputValue === value"
    >
      <FontAwesomeIcon :icon="['fas', 'floppy-disk']" />
    </button>
  </form>
</template>

<style scoped>
@reference "tailwindcss";

.bandwidth-control {
  @apply flex gap-2 items-center;
}

.bandwidth-input {
  @apply px-2 py-1 w-24 text-sm border rounded
    bg-white dark:bg-neutral-700
    border-neutral-300 dark:border-neutral-600
    focus:border-blue-500 dark:focus:border-blue-400
    focus:ring-1 focus:ring-blue-500 dark:focus:ring-blue-400
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
  @apply px-3 py-1 text-sm rounded
    bg-blue-500 hover:bg-blue-600
    text-white
    disabled:opacity-50 disabled:cursor-not-allowed
    transition-colors duration-200;
}
</style>
