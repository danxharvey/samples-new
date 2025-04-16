'use client'

import { useState } from 'react'
import Link from 'next/link'
import Image from 'next/image'
import { FiMenu, FiX } from 'react-icons/fi'
import { usePathname } from 'next/navigation'
import ThemeSwitch from './ThemeSwitch'

const navItems = [
  { href: '/', label: 'Home' },
  { href: '/portfolio', label: 'Portfolio' },
  { href: '/about', label: 'About' },
  { href: '/contact', label: 'Contact' },
]

export default function Navbar() {
  const [isMenuOpen, setIsMenuOpen] = useState(false)
  const pathname = usePathname()

  // Helper function to determine the link's active state
  const linkClasses = (href: string) =>
    `transition-colors ${
      pathname === href || (href === '/portfolio' && pathname.startsWith('/portfolio/'))
        ? 'text-primary-light dark:text-primary-dark font-medium'
        : 'text-gray-700 dark:text-gray-200 hover:text-primary-light dark:hover:text-primary-dark'
    }`

  // Render navigation links
  const renderLinks = (isMobile = false) =>
    navItems.map(({ href, label }) => (
      <Link
        key={href}
        href={href}
        className={linkClasses(href)}
        onClick={isMobile ? () => setIsMenuOpen(false) : undefined}
      >
        {label}
      </Link>
    ))

  return (
    <nav className="bg-white dark:bg-gray-900 shadow-sm sticky top-0 z-50">
      <div className="container-custom py-4">
        <div className="flex justify-between items-center">
          {/* Logo and Home link */}
          <Link href="/" className="flex items-center">
            <div className="relative w-32 h-10 mr-2">
              <Image
                src="/images/favicon.ico"
                alt="Neuron 5 Logo"
                fill
                priority
                sizes="(max-width: 768px) 100vw, 160px"
                className="object-contain"
              />
            </div>
          </Link>

          {/* Desktop Navigation */}
          <div className="hidden md:flex items-center space-x-8">
            {renderLinks()}
            <ThemeSwitch />
          </div>

          {/* Mobile Navigation */}
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

        {/* Mobile Menu */}
        {isMenuOpen && (
          <div className="md:hidden pt-4 pb-2">
            <div className="flex flex-col space-y-4">{renderLinks(true)}</div>
          </div>
        )}
      </div>
    </nav>
  )
}
