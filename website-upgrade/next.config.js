/** @type {import('next').NextConfig} */
const nextConfig = {
    reactStrictMode: true,
    images: {
      domains: ['hostinger.co.uk'],
    },
    typescript: {
      ignoreBuildErrors: true,
    },
    // output: 'export',
  }
  
  module.exports = nextConfig