<script setup>
import AlertDialog from './components/AlertDialog.vue'
import BridgePannel from './components/bridge/Pannel.vue'
import TopicPannel from './components/topic/Pannel.vue'
</script>

<template>
  <main class="hub-col">
    <AlertDialog>
      <p class="text-center">Aggregated stats</p>
      <p class="text-center">5.2 MB/s Dispatched &mdash; 3.4 MB/s Forwarded</p>
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
</template>

<style scoped></style>
