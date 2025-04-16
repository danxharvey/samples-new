/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './app/**/*.{js,ts,jsx,tsx,mdx}',
    './pages/**/*.{js,ts,jsx,tsx,mdx}',
    './components/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  darkMode: 'class',
  theme: {
    extend: {
      colors: {
        primary: {
          light: '#a04732', // Rust accent (Aux1)
          dark: '#a04732',  // Same for dark mode
        },
        background: {
          light: '#edeae2', // Cream background
          dark: '#02000b',  // Almost black background
        },
        text: {
          light: '#02000b', // Almost black for text
          dark: '#edeae2',  // Cream for dark mode text
        }
      },
      fontFamily: {
        sans: ['var(--font-roboto)', 'sans-serif'],
        heading: ['var(--font-caudex)', 'serif'],
      },
    },
  },
  plugins: [],
}
