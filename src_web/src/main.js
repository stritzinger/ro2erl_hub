import './assets/main.css'

import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { library } from '@fortawesome/fontawesome-svg-core'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'

import {
  faPlus,
  faCheck,
  faFloppyDisk,
} from '@fortawesome/free-solid-svg-icons'

// Add icons to the library
library.add(faPlus, faCheck, faFloppyDisk)
import App from './App.vue'

const app = createApp(App)

app.use(createPinia())
app.component('FontAwesomeIcon', FontAwesomeIcon)
app.mount('#app')
