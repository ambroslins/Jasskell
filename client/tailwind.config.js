/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.elm",
    "../server/src/Jasskell/Server/Page.hs"
  ],
  theme: {
    extend: {
      aspectRatio: {
        card: '9 / 16'
      }
    },
  },
  plugins: [],
  purge: false,
}
