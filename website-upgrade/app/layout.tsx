'use client'

import './globals.css'
import { Roboto, Caudex } from 'next/font/google'
import { ThemeProvider } from '@/components/theme-provider'
import Navbar from '@/components/Navbar'
import Footer from '@/components/Footer'
import { metadata } from './metadata'

// Define the Roboto font for paragraph text
const roboto = Roboto({
  weight: ['400', '500', '700'],
  subsets: ['latin'],
  variable: '--font-roboto',
  display: 'swap',
})

// Define Caudex for headings
const caudex = Caudex({
  weight: ['400', '700'],
  subsets: ['latin'],
  variable: '--font-caudex',
  display: 'swap',
})

export default function RootLayout({ children }: { children: React.ReactNode }) {
  
  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <title>{metadata.title}</title>
        <meta name="description" content={metadata.description} />
        <meta name="author" content={metadata.author} />
        <meta name="keywords" content={metadata.keywords.join(', ')} />
        <meta name="robots" content={metadata.robots} />
        <meta name="canonical" content={metadata.canonical} />
        <meta property="og:title" content={metadata.openGraph.title} />
        <meta property="og:description" content={metadata.openGraph.description} />
      </head>
      <body className={`${roboto.variable} ${caudex.variable} font-sans`}>
        <ThemeProvider attribute="class">
          <div className="flex flex-col min-h-screen">
            <Navbar />
            <main className="flex-grow">
              {children}
            </main>
            <Footer />
          </div>
        </ThemeProvider>
      </body>
    </html>
  )
}
