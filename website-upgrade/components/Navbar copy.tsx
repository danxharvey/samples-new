'use client'

import { useState, useEffect } from 'react'
import Link from 'next/link'
import Image from 'next/image'
import { FiMenu, FiX } from 'react-icons/fi'
import { useTheme } from 'next-themes'
import ThemeSwitch from './ThemeSwitch'
import { usePathname } from 'next/navigation'

export default function Navbar() {
  const [isMenuOpen, setIsMenuOpen] = useState(false)
  const [mounted, setMounted] = useState(false)
  const { theme } = useTheme()
  const pathname = usePathname()
  
  // Add this useEffect to handle the mounted state
  useEffect(() => {
    setMounted(true)
  }, [])
  
  // Determine if dark mode is active
  const isDarkMode = mounted && theme === 'dark'
  
  return (
    <nav className="bg-white dark:bg-gray-900 shadow-sm sticky top-0 z-50">
      <div className="container-custom py-4">
        <div className="flex justify-between items-center">
          {/* Logo */}
          <Link href="/" className="flex items-center">
            <div className="relative w-40 h-12 mr-2">
              {mounted ? (
                <Image
                  src={isDarkMode ? "/images/favicon.ico" : "/images/favicon.ico"}
                  alt="Neuron 5 Logo"
                  fill
                  priority
                  sizes="(max-width: 768px) 100vw, 160px"
                  className="object-contain"
                />
              ) : (
                <div className="absolute inset-0 flex items-center justify-center">
                  <span className="text-primary-light dark:text-primary-dark font-bold text-xl">NEURON 5</span>
                </div>
              )}
            </div>
          </Link>
          
          {/* Desktop Navigation */}
          <div className="hidden md:flex items-center space-x-8">
            <Link 
              href="/" 
              className={`transition-colors ${
                pathname === '/' 
                  ? 'text-primary-light dark:text-primary-dark font-medium' 
                  : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
              }`}
            >
              Home
            </Link>
            <Link 
              href="/portfolio" 
              className={`transition-colors ${
                pathname === '/portfolio' || pathname.startsWith('/portfolio/') 
                  ? 'text-primary-light dark:text-primary-dark font-medium' 
                  : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
              }`}
            >
              Portfolio
            </Link>
            <Link 
              href="/about" 
              className={`transition-colors ${
                pathname === '/about' 
                  ? 'text-primary-light dark:text-primary-dark font-medium' 
                  : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
              }`}
            >
              About
            </Link>
            <Link 
              href="/contact" 
              className={`transition-colors ${
                pathname === '/contact' 
                  ? 'text-primary-light dark:text-primary-dark font-medium' 
                  : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
              }`}
            >
              Contact
            </Link>
            <ThemeSwitch />
          </div>
          
          {/* Mobile Navigation Button */}
          <div className="flex md:hidden items-center">
            <ThemeSwitch />
            <button 
              onClick={() => setIsMenuOpen(!isMenuOpen)}
              className="ml-4 text-gray-700 dark:text-gray-200"
            >
              {isMenuOpen ? <FiX className="w-6 h-6" /> : <FiMenu className="w-6 h-6" />}
            </button>
          </div>
        </div>
        
        {/* Mobile Navigation Menu */}
        {isMenuOpen && (
          <div className="md:hidden pt-4 pb-2">
            <div className="flex flex-col space-y-4">
              <Link 
                href="/" 
                className={`transition-colors ${
                  pathname === '/' 
                    ? 'text-primary-light dark:text-primary-dark font-medium' 
                    : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
                }`}
                onClick={() => setIsMenuOpen(false)}
              >
                Home
              </Link>
              <Link 
                href="/portfolio" 
                className={`transition-colors ${
                  pathname === '/portfolio' || pathname.startsWith('/portfolio/') 
                    ? 'text-primary-light dark:text-primary-dark font-medium' 
                    : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
                }`}
                onClick={() => setIsMenuOpen(false)}
              >
                Portfolio
              </Link>
              <Link 
                href="/about" 
                className={`transition-colors ${
                  pathname === '/about' 
                    ? 'text-primary-light dark:text-primary-dark font-medium' 
                    : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
                }`}
                onClick={() => setIsMenuOpen(false)}
              >
                About
              </Link>
              <Link 
                href="/contact" 
                className={`transition-colors ${
                  pathname === '/contact' 
                    ? 'text-primary-light dark:text-primary-dark font-medium' 
                    : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
                }`}
                onClick={() => setIsMenuOpen(false)}
              >
                Contact
              </Link>
            </div>
          </div>
        )}
      </div>
    </nav>
  )
}
