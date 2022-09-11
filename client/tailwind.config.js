/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.elm"],
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
