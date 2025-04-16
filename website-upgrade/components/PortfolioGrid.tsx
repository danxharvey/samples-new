'use client'

import { useState } from 'react'
import Link from 'next/link'
import { FiExternalLink } from 'react-icons/fi'
import { projects } from '@/data/projects'
import Image from 'next/image'

// Dynamically extract unique categories
const categories = [
  'All', ...Array.from(new Set(projects.map((project) => project.category))).sort()
];

export default function PortfolioGrid() {
  const [filter, setFilter] = useState('All')
  
  // Filter projects based on selected category
  const filteredProjects = filter === 'All' 
    ? projects 
    : projects.filter(project => project.category === filter)
  
  return (
    <div>
      {/* Filter buttons */}
      <div className="flex flex-wrap gap-2 mb-8">
        {categories.map(category => (
          <button
            key={category}
            onClick={() => setFilter(category)}
            className={`px-4 py-2 rounded-md transition-colors ${
              filter === category
                ? 'bg-primary-light dark:bg-primary-light text-white'
                : 'bg-gray-100 dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-700'
            }`}
          >
            {category}
          </button>
        ))}
      </div>
      
      {/* Project grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
        {filteredProjects.map(project => (
          <div 
            key={project.id}
            className="group bg-white dark:bg-gray-900 rounded-lg overflow-hidden shadow-sm hover:shadow-md transition-shadow flex flex-col h-full"
          >
            {/* Project image placeholder */}
            <div className="h-48 bg-blue-100 dark:bg-white-900 flex items-center justify-center">
              {/* <span className="text-blue-500 dark:text-blue-300 text-lg">Project Image</span> */}
              <Image
                src={project.image}  // Replace with the actual image path
                alt={project.title}
                layout='intrinsic'  // Use 'intrinsic' for aspect ratio based on the original image size
                width={470}         // Adjust width as needed
                height={235}        // Adjust height as needed
                className='object-contain'  // Ensures image is contained within the box without distortion
              />
            </div>
            
            {/* Project details */}
            <div className="p-6 flex flex-col flex-grow">
              <div className="mb-2 text-sm font-medium text-primary-light dark:text-primary-light">
                {project.category}
              </div>
              <h3 className="text-xl font-semibold font-heading text-gray-900 dark:text-white mb-3 group-hover:text-primary-light dark:group-hover:text-primary-light transition-colors">
                {project.title}
              </h3>
              <p className="text-gray-600 dark:text-gray-300 mb-4 flex-grow">
                {project.description}
              </p>
              {project.ready ? (
                <Link
                  href={`/portfolio/${project.topic}/${project.slug}`}
                  className="inline-flex items-center text-primary-light dark:text-primary-light hover:underline mt-auto"
                >
                  View Details <FiExternalLink className="ml-2" />
                </Link>
              ) : (
                <span>&nbsp;</span>
              )}
            </div>
          </div>
        ))}
      </div>
    </div>
  )
}
