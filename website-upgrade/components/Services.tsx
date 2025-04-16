 'use client'

import { FiDatabase, FiBarChart2, FiEye } from 'react-icons/fi'
import { GiBrain } from 'react-icons/gi'

const services = [
  {
    title: 'Data Engineering',
    description: 'Build robust automated pipelines and infrastructure for efficient data processing.',
    icon: FiDatabase,
  },
  {
    title: 'Business Intelligence',
    description: 'Transform your data into actionable insights with analytics and visualization.',
    icon: FiBarChart2,
  },
  {
    title: 'Data Science & AI',
    description: 'Implement advanced algorithms and AI Tools to unlock your potential.',
    icon: GiBrain,
  },
  {
    title: 'Research & Analysis',
    description: 'Highly analytical with research and critical thinking skills to turbo charge your business.',
    icon: FiEye,
  }
]

export default function Services() {
  return (
    <section className="py-16 bg-gray-50 dark:bg-gray-800">
      <div className="container-custom">
        <div className="text-center mb-16">
          <h2 className="heading-2 text-gray-900 dark:text-white mb-4">Our Services</h2>
          <p className="text-lg text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
            We provide end-to-end solutions from data infrastructure to advanced AI applications
          </p>
        </div>
        
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8">
          {services.map((service, index) => (
            <div 
              key={index}
              className="bg-white dark:bg-gray-900 p-8 rounded-lg shadow-sm hover:shadow-md transition-shadow flex flex-col items-center justify-between"
            >
              {/* Icon and Title Section */}
              <div className="flex flex-col items-center mb-5">
                <div className="text-primary-light dark:text-primary-dark mb-3">
                  <service.icon className="w-12 h-12" />
                </div>
                <h3 className="text-xl font-semibold text-gray-900 dark:text-white text-center">
                  {service.title}
                </h3>
              </div>

              {/* Description Section */}
              <p className="text-gray-600 dark:text-gray-300 text-center flex-1">
                {service.description}
              </p>
            </div>
          ))}
        </div>

      </div>
    </section>
  )
}
