import PortfolioGrid from '@/components/PortfolioGrid'
import PortfolioCarousel from '@/components/PortfolioCarousel'

export default function Portfolio() {
  return (
    <div className="py-16">
      
      {/* Hero section with gradient background - similar to contact page */}
      <div className="relative h-64 mb-16 overflow-hidden">
        <div className="absolute inset-0 bg-gradient-to-r from-primary-light via-blue-500 to-purple-600"></div>
        <div className="absolute inset-0 opacity-20 bg-[url('/grid-pattern.svg')]"></div>
        <div className="container-custom h-full flex items-center relative z-10">
          <div className="max-w-2xl">
            <h1 className="heading-1 text-white mb-4 font-heading">Portfolio Summary</h1>
            <p className="text-xl text-white/90">
              Links will be added as projects are written up.
            </p>
          </div>
        </div>
      </div>
      
      <div className="container-custom">
        {/* Featured Projects Carousel */}
        <div className="mb-20">
          <h2 className="heading-2 text-gray-900 dark:text-white mb-8 font-heading">Current Projects</h2>
          <PortfolioCarousel />
        </div>
        
        {/* All Projects Grid */}
        <div>
          <h2 className="heading-2 text-gray-900 dark:text-white mb-8 font-heading">Featured Projects</h2>
          <PortfolioGrid />
        </div>
      </div>

    </div>
  )
}
