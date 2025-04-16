import Link from 'next/link'
import { FiArrowLeft, FiCalendar, FiTag, FiUser } from 'react-icons/fi'

// Sample data - in a real app, this would come from a CMS
const projects = {
  'predictive-maintenance': {
    title: 'AI-Powered Predictive Maintenance',
    topic: 'machine-learning',
    topicTitle: 'Machine Learning',
    description: 'Machine learning system that predicts equipment failures before they happen.',
    fullDescription: `
      <p>Our AI-powered predictive maintenance solution leverages machine learning algorithms to analyze sensor data from industrial equipment, identifying patterns that precede mechanical failures.</p>
      
      <p>By detecting subtle changes in vibration, temperature, and other parameters, our system can alert maintenance teams to potential issues days or weeks before they lead to costly downtime.</p>
      
      <h3>Key Features</h3>
      <ul>
        <li>Real-time sensor data processing</li>
        <li>Anomaly detection using supervised and unsupervised learning</li>
        <li>Maintenance prioritization based on failure probability</li>
        <li>Integration with existing maintenance management systems</li>
      </ul>
      
      <h3>Technologies Used</h3>
      <ul>
        <li>TensorFlow for model development</li>
        <li>Time-series analysis with LSTM networks</li>
        <li>Apache Kafka for real-time data streaming</li>
        <li>Custom dashboards for maintenance teams</li>
      </ul>
    `,
    client: 'Industrial Manufacturing Company',
    date: 'January 2023',
    tags: ['Machine Learning', 'Predictive Analytics', 'IoT', 'Industry 4.0']
  },
  'quality-control': {
    title: 'Computer Vision for Quality Control',
    topic: 'computer-vision',
    topicTitle: 'Computer Vision',
    description: 'Automated visual inspection system for manufacturing defect detection.',
    fullDescription: `
      <p>Content to be added</p>
    `,
    client: 'Electronics Manufacturer',
    date: 'March 2023',
    tags: ['Computer Vision', 'Quality Assurance', 'Manufacturing']
  },
  // Add similar entries for other projects
}

export function generateStaticParams() {
  return Object.entries(projects).map(([slug, project]) => ({
    topic: project.topic,
    slug,
  }))
}

export default function ProjectPage({ params }: { params: { topic: string, slug: string } }) {
  const project = projects[params.slug as keyof typeof projects]
  
  // Fallback for projects that don't exist
  if (!project) {
    return (
      <div className="py-16">
        <div className="container-custom">
          <div className="max-w-4xl mx-auto">
            <p>Project not found.</p>
            <Link href="/portfolio" className="text-primary-light dark:text-primary-light hover:underline">
              Return to Portfolio
            </Link>
          </div>
        </div>
      </div>
    )
  }
  
  return (
    <div className="py-16">
      <div className="container-custom">
        <div className="max-w-4xl mx-auto">
          <Link href={`/portfolio/${params.topic}`} className="inline-flex items-center text-primary-light dark:text-primary-light hover:underline mb-6">
            <FiArrowLeft className="mr-2" /> Back to {project.topicTitle}
          </Link>
          
          <h1 className="heading-1 text-gray-900 dark:text-white mb-4 font-heading">{project.title}</h1>
          
          <div className="flex flex-wrap gap-4 mb-8 text-sm text-gray-600 dark:text-gray-400">
            {project.client && (
              <div className="flex items-center">
                <FiUser className="mr-2" />
                <span>{project.client}</span>
              </div>
            )}
            {project.date && (
              <div className="flex items-center">
                <FiCalendar className="mr-2" />
                <span>{project.date}</span>
              </div>
            )}
          </div>
          
          {/* Project image placeholder */}
          <div className="h-80 bg-blue-100 dark:bg-blue-900 rounded-lg mb-8 flex items-center justify-center">
            <span className="text-blue-500 dark:text-blue-300 text-lg">Project Image</span>
          </div>
          
          <div className="prose prose-lg dark:prose-invert max-w-none mb-8">
            <div dangerouslySetInnerHTML={{ __html: project.fullDescription }} />
          </div>
          
          {project.tags && (
            <div className="mt-12 pt-6 border-t border-gray-200 dark:border-gray-700">
              <div className="flex items-center flex-wrap gap-2">
                <FiTag className="text-gray-500 dark:text-gray-400 mr-2" />
                {project.tags.map(tag => (
                  <span 
                    key={tag}
                    className="px-3 py-1 bg-gray-100 dark:bg-gray-800 text-gray-700 dark:text-gray-300 rounded-full text-sm"
                  >
                    {tag}
                  </span>
                ))}
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
