<script setup>
import { onMounted, onBeforeMount, ref, watch, computed } from 'vue'
import { storeToRefs } from 'pinia'
import AlertDialog from './components/AlertDialog.vue'
import BridgePannel from './components/bridge/Pannel.vue'
import TopicPannel from './components/topic/Pannel.vue'
import Toast from './components/Toast.vue'
import { useRo2erlHubStore } from './stores/ro2erlHub'
import { useDataStore } from './stores/data'
const ro2erlHubStore = useRo2erlHubStore()
const { isOpen } = storeToRefs(ro2erlHubStore)
const { getDispatchedStats, getForwardedStats } = useDataStore()
const token = ref(null)
const loading = ref(true)

onBeforeMount(() => {
  if (typeof window !== 'undefined') {
    const params = new URLSearchParams(window.location.search)
    token.value = params.get('token')
  }
})

onMounted(() => {
  ro2erlHubStore.initStateAndWS(token.value)
})

watch(isOpen, () => {
  loading.value = false
})

const dispatchedStats = computed(() => {
  return getDispatchedStats()
})

const forwardedStats = computed(() => {
  return getForwardedStats()
})
</script>

<template>
  <main v-if="isOpen && token" class="hub-col relative">
    <Toast />
    <AlertDialog>
      <p class="text-center">Aggregated stats</p>
      <p class="text-center">
        <span class="font-code">{{ dispatchedStats }} MB/s</span>
        Dispatched &mdash;
        <span class="font-code">{{ forwardedStats }} MB/s</span>
        Forwarded
      </p>
    </AlertDialog>
    <div class="hub-container">
      <div class="hub-row">
        <div class="w-full md:w-1/3">
          <BridgePannel title="Bridges" />
        </div>
        <div class="w-full md:w-2/3">
          <TopicPannel title="Topics" />
        </div>
      </div>
    </div>
  </main>
  <main v-else class="flex justify-center mt-16">
    <div class="w-fit">
      <AlertDialog v-if="loading" color="grisp-50">
        <p class="flex items-center gap-2">
          <FontAwesomeIcon :icon="['fas', 'gear']" spin />
          Loading
        </p>
      </AlertDialog>
      <AlertDialog v-else color="red-100">
        <p class="text-center">
          <FontAwesomeIcon :icon="['fas', 'exclamation-triangle']" />
          Oops! Invalid token, you can't access ro2erl-hub
        </p>
      </AlertDialog>
    </div>
  </main>
</template>

<style scoped></style>
