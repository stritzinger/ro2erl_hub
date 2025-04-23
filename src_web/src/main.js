import './assets/main.css'

import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { library } from '@fortawesome/fontawesome-svg-core'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'

import {
  faCheck,
  faXmark,
  faExclamationTriangle,
  faCircle,
  faGear,
} from '@fortawesome/free-solid-svg-icons'

// Add icons to the library
library.add(faCheck, faXmark, faExclamationTriangle, faCircle, faGear)
import App from './App.vue'

const app = createApp(App)

app.use(createPinia())
app.component('FontAwesomeIcon', FontAwesomeIcon)
app.mount('#app')
