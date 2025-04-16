'use client'

import { useState, useEffect } from 'react'
import Link from 'next/link'
import { FiChevronLeft, FiChevronRight } from 'react-icons/fi'
import Image from 'next/image'
import { featuredProjects } from '@/data/featured-projects'

export default function PortfolioCarousel() {
  const [currentIndex, setCurrentIndex] = useState(0)
  
  const nextSlide = () => {
    setCurrentIndex((prevIndex) => (prevIndex + 1) % featuredProjects.length)
  }
  
  const prevSlide = () => {
    setCurrentIndex((prevIndex) => 
      prevIndex === 0 ? featuredProjects.length - 1 : prevIndex - 1
    )
  }
  
  useEffect(() => {
    // If you have auto-advance functionality
    const timer = setInterval(() => {
      nextSlide();
    }, 7500);
    
    // Cleanup function
    return () => clearInterval(timer);
  }, []);
  
  return (
    <div className="relative overflow-hidden rounded-lg bg-black dark:bg-black">
      {/* Carousel slides */}
      <div 
        className="relative h-[500px] transition-all duration-750 ease-in-out"
        style={{ transform: `translateX(-${currentIndex * 100}%)` }}
      >
        <div className="absolute inset-0 flex">
          {featuredProjects.map((project, index) => (
            <div 
              key={project.id}
              className="relative w-full h-full flex-shrink-0"
            >
              {/* Image placeholder */}
              <div className="absolute inset-0 bg-black dark:bg-black flex items-center justify-center">
                {/* <span className="text-blue-500 dark:text-blue-300 text-lg">Project Image {index + 1}</span> */}
                <Image
                  src={project.image}  // Replace with the actual image path
                  alt={project.title}
                  layout='intrinsic'  // Use 'intrinsic' for aspect ratio based on the original image size
                  width={820}         // Adjust width as needed
                  height={410}        // Adjust height as needed
                  className='object-contain'  // Ensures image is contained within the box without distortion
              />

              </div>
              
              {/* Content overlay */}
              <div className="absolute inset-0 bg-gradient-to-t from-black/70 to-transparent flex items-end">
                <div className="p-8 text-white">
                  <div className="mb-2 text-blue-300 font-medium">{project.category}</div>
                  <h3 className="text-2xl font-bold mb-2 font-heading">{project.title}</h3>
                  <p className="text-gray-200 mb-6">{project.description}</p>
                  {project.ready ? (
                    <Link
                      href={`/portfolio/${project.topic}/${project.slug}`}
                      className="px-6 py-2 bg-primary-light dark:bg-primary-light rounded-md hover:opacity-90 transition-opacity inline-block"
                    >
                      View Project
                    </Link>
                    ) : (
                      <span className="px-6 py-2 bg-gray-600 dark:bg-gray-600 rounded-md hover:opacity-80 transition-opacity inline-block">
                        Coming Soon
                      </span>
                    )}
                </div>
              </div>
            </div>
          ))}
        </div>
      </div>
      
      {/* Carousel controls */}
      <button 
        onClick={prevSlide}
        className="absolute left-4 top-1/3 -translate-y-1/2 p-2 rounded-full bg-white/30 text-white hover:bg-white/50 transition-colors"
        aria-label="Previous slide"
      >
        <FiChevronLeft className="w-6 h-6" />
      </button>
      
      <button 
        onClick={nextSlide}
        className="absolute right-4 top-1/3 -translate-y-1/2 p-2 rounded-full bg-white/30 text-white hover:bg-white/50 transition-colors"
        aria-label="Next slide"
      >
        <FiChevronRight className="w-6 h-6" />
      </button>
      
      {/* Carousel indicators */}
      <div className="absolute bottom-4 left-1/2 -translate-x-1/2 flex space-x-2">
        {featuredProjects.map((_, index) => (
          <button
            key={index}
            onClick={() => setCurrentIndex(index)}
            className={`w-3 h-3 rounded-full ${
              index === currentIndex ? 'bg-white' : 'bg-white/50'
            }`}
            aria-label={`Go to slide ${index + 1}`}
          />
        ))}
      </div>
    </div>
  )
}