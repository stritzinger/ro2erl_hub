import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import vueDevTools from 'vite-plugin-vue-devtools'
import tailwindcss from '@tailwindcss/vite'

// https://vite.dev/config/
export default ({ mode }) => {
  return defineConfig({
    plugins: [vue(), vueDevTools(), tailwindcss()],
    logLevel: 'warn',
    resolve: {
      alias: {
        '@': fileURLToPath(new URL('./src', import.meta.url)),
      },
    },
    build: {
      emptyOutDir: true,
      sourcemap: mode !== 'production',
      minify: mode === 'production',
      outDir: fileURLToPath(new URL('../priv/web/static', import.meta.url)),
    },
  })
}
