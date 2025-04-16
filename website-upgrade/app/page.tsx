import Hero from '@/components/Hero'
import Services from '@/components/Services'
import CTA from '@/components/CTA'
import PortfolioGrid from '@/components/PortfolioGrid'
import PortfolioCarousel from '@/components/PortfolioCarousel'
import PageHeader from '@/components/PageHeader'

export default function Home() {
  return (
    <div className="space-y-20 py-10">
      <Hero />
      <Services />
      <CTA />
    </div>
  )
}

export function Portfolio() {
  return (
    <div className="py-16">
      <PageHeader 
        title="Our Portfolio" 
        description="Explore our latest projects and AI solutions across various industries"
      />
      
      <div className="container-custom">

        {/* Featured Projects Carousel */}
        <div className="mb-20">
          <h2 className="heading-2 text-gray-900 dark:text-white mb-8">Featured Projects</h2>
          <PortfolioCarousel />
        </div>
        {/* All Projects Grid */}
        <div>
          <h2 className="heading-2 text-gray-900 dark:text-white mb-8">All Projects</h2>
          <PortfolioGrid />
        </div>

      </div>
    </div>
  )
}
