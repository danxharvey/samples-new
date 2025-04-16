import Link from 'next/link'
import { FiArrowLeft, FiExternalLink } from 'react-icons/fi'
import PageHeader from '@/components/PageHeader'

// Sample data - in a real app, this would come from a CMS or data source
const topics = {
  'ML': {
    title: 'Machine Learning',
    description: 'Our machine learning projects leverage advanced algorithms to create predictive models and pattern recognition systems.',
    projects: [
      {
        id: 7,
        title: 'AI-Powered Predictive Maintenance',
        description: 'Machine learning system that predicts equipment failures before they happen.',
        slug: 'predictive-maintenance'
      },
      {
        id: 9,
        title: 'Time Series Forecasting',
        description: 'Advanced models for predicting future trends based on historical data.',
        slug: 'time-series'
      }
    ]
  },
  'DL': {
    title: 'Computer Vision',
    description: 'Our computer vision solutions enable machines to interpret and understand visual information from the world.',
    projects: [
      {
        id: 8,
        title: 'Computer Vision for Quality Control',
        description: 'Automated visual inspection system for manufacturing defect detection.',
        slug: 'quality-control'
      }
    ]
  },
  'machine-learning': {
    title: 'Machine Learning',
    description: 'Our machine learning projects leverage advanced algorithms to create predictive models and pattern recognition systems.',
    projects: [
      {
        id: 1,
        title: 'AI-Powered Predictive Maintenance',
        description: 'Machine learning system that predicts equipment failures before they happen.',
        slug: 'predictive-maintenance'
      },
      {
        id: 6,
        title: 'Time Series Forecasting',
        description: 'Advanced models for predicting future trends based on historical data.',
        slug: 'time-series'
      }
    ]
  },
  'computer-vision': {
    title: 'Computer Vision',
    description: 'Our computer vision solutions enable machines to interpret and understand visual information from the world.',
    projects: [
      {
        id: 2,
        title: 'Computer Vision for Quality Control',
        description: 'Automated visual inspection system for manufacturing defect detection.',
        slug: 'quality-control'
      }
    ]
  },
  'data-engineering': {
    title: 'Data Engineering',
    description: 'Our data engineering projects focus on building robust data pipelines and infrastructure for efficient data processing.',
    projects: [
      {
        id: 3,
        title: 'Data Pipeline Optimization',
        description: 'Scalable data infrastructure for real-time analytics processing.',
        slug: 'pipeline-optimization'
      }
    ]
  },
  'business-intelligence': {
    title: 'Business Intelligence',
    description: 'Our BI solutions transform data into actionable insights through advanced analytics and visualization.',
    projects: [
      {
        id: 4,
        title: 'Customer Segmentation Platform',
        description: 'Advanced clustering algorithms for targeted marketing campaigns.',
        slug: 'customer-segmentation'
      }
    ]
  },
  'nlp': {
    title: 'Natural Language Processing',
    description: 'Our NLP systems analyze and understand human language to extract meaning and insights.',
    projects: [
      {
        id: 5,
        title: 'Natural Language Processing System',
        description: 'Text analysis system for customer feedback categorization.',
        slug: 'text-analysis'
      }
    ]
  }
}

export function generateStaticParams() {
  return Object.keys(topics).map(topic => ({
    topic,
  }))
}

export default function TopicPage({ params }: { params: { topic: string } }) {
  const topic = topics[params.topic as keyof typeof topics]
  
  // Fallback for topics that don't exist
  if (!topic) {
    return (
      <div className="py-16">
        <PageHeader title="Topic Not Found" />
        <div className="container-custom">
          <div className="max-w-4xl mx-auto">
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
      <PageHeader 
        title={topic.title} 
        description={topic.description}
      />
      
      <div className="container-custom">
        <div className="max-w-4xl mx-auto">
          <Link href="/portfolio" className="inline-flex items-center text-primary-light dark:text-primary-light hover:underline mb-6">
            <FiArrowLeft className="mr-2" /> Back to Portfolio
          </Link>
          
          <h2 className="heading-2 text-gray-900 dark:text-white mb-8 font-heading">Projects</h2>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {topic.projects.map(project => (
              <div 
                key={project.id}
                className="bg-white dark:bg-gray-900 rounded-lg overflow-hidden shadow-sm hover:shadow-md transition-shadow flex flex-col h-full"
              >
                {/* Project image placeholder */}
                <div className="h-48 bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                  <span className="text-blue-500 dark:text-blue-300 text-lg">Project Image</span>
                </div>
                
                {/* Project details */}
                <div className="p-6 flex flex-col flex-grow">
                  <h3 className="text-xl font-semibold font-heading text-gray-900 dark:text-white mb-3">
                    {project.title}
                  </h3>
                  <p className="text-gray-600 dark:text-gray-300 mb-4 flex-grow">
                    {project.description}
                  </p>
                  <Link
                    href={`/portfolio/${params.topic}/${project.slug}`}
                    className="inline-flex items-center text-primary-light dark:text-primary-light hover:underline mt-auto"
                  >
                    View Details <FiExternalLink className="ml-2" />
                  </Link>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}
