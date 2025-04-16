import Link from 'next/link'
import { FaLinkedin } from 'react-icons/fa'
import Image from 'next/image'

export default function Footer() {
  return (
    <footer className="bg-gray-100 dark:bg-gray-900 py-12">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-12 text-center">
          <div>
            <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-4">Neuron 5</h3>
            <p className="text-gray-600 dark:text-gray-300">South Queensferry</p>
            <p className="text-gray-600 dark:text-gray-300">Greater Edinburgh</p>
            <p className="text-gray-600 dark:text-gray-300">Scotland</p>
          </div>
          
          <div>
            <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-4">Contact</h3>
            <p className="text-gray-600 dark:text-gray-300">
              <a href="mailto:hello@neuron5.co.uk" className="text-primary-light dark:text-primary-dark hover:underline">
                hello@neuron5.co.uk
              </a>
            </p>
            <p className="text-gray-600 dark:text-gray-300">UK: +44 7899 702 453</p>
            <p className="text-gray-600 dark:text-gray-300">IRL: +353 86 056 9070</p>
          </div>
          
          <div>
            <span className="text-xl font-semibold text-gray-900 dark:text-white mb-0 hover:text-primary-light dark:hover:text-primary-light">
            <a 
              href="https://linkedin.com/company/neuron5" 
              target="_blank" 
              rel="noopener noreferrer"
              className="group flex items-center justify-center"
            >
              <FaLinkedin className="w-8 h-8 mr-3"/>
              <h3>Neuron 5</h3>
            </a>
            </span>
            <div className="flex justify-center mt-6">
              <Image
                src="/images/favicon.ico"
                alt="Neuron 5 logo"
                width={64}
                height={64}
              />
            </div>
          </div>
        </div>
        
        <div className="border-t border-gray-200 dark:border-gray-700 mt-12 pt-8 text-center">
          <p className="text-gray-500 dark:text-gray-400">
            &copy; {new Date().getFullYear()} Neuron 5 Ltd. All rights reserved.
          </p>
        </div>
      </div>
    </footer>
  )
}
