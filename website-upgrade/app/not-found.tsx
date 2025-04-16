import Link from 'next/link'

export default function NotFound() {
  return (
    <div className="py-20">
      <div className="container-custom">
        <div className="max-w-3xl mx-auto text-center">
          <div className="mb-8">
            {/* 404 placeholder illustration */}
            <div className="relative w-48 h-48 mx-auto mb-6 rounded-full bg-black dark:bg-white flex items-center justify-center">
              <span className="text-6xl font-bold text-primary-light dark:text-primary-dark">404</span>
            </div>
            
            <h1 className="heading-1 text-gray-900 dark:text-white mb-4">
              Page Not Found
            </h1>
            
            <p className="text-lg text-gray-600 dark:text-gray-300 mb-10">
              Sorry, we couldn't find the page you're looking for.
            </p>
            
            <div>
              <Link 
                href="/"
                className="btn-primary inline-flex items-center"
              >
                Return to Homepage
              </Link>
            </div>
          </div>
          
          <div className="pt-10 border-t border-gray-200 dark:border-gray-700">
            <p className="text-gray-500 dark:text-gray-400">
              If you believe this is an error, please contact us at{' '}
              <a 
                href="mailto:hello@neuron5.co.uk" 
                className="text-primary-light dark:text-primary-dark hover:underline"
              >
                hello@neuron5.co.uk
              </a>
            </p>
          </div>
        </div>
      </div>
    </div>
  )
}
