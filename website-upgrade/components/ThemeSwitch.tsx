'use client'

import { useTheme } from 'next-themes'
import { useEffect, useState } from 'react'
import { FiSun, FiMoon } from 'react-icons/fi'

export default function ThemeSwitch() {
  const [mounted, setMounted] = useState(false)
  const { theme, setTheme } = useTheme()

  // Ensures that theme rendering logic only happens on the client side
  useEffect(() => {
    setMounted(true) // Set mounted to true once the component has mounted
  }, [])

  // If the component hasn't mounted yet, don't render the theme switch (prevents SSR mismatch)
  if (!mounted) {
    return null
  }

  // Render the theme switch button with the current theme
  return (
    <button
      aria-label="Toggle Dark Mode"
      type="button"
      className="p-2 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 transition-colors"
      onClick={() => setTheme(theme === 'dark' ? 'light' : 'dark')}
    >
      {theme === 'dark' ? (
        <FiSun className="w-5 h-5" /> // Sun icon for dark mode
      ) : (
        <FiMoon className="w-5 h-5" /> // Moon icon for light mode
      )}
    </button>
  )
}
