import CTA2 from '@/components/CTA2'
import Image from 'next/image'
import Link from 'next/link'
import { FaLinkedin, FaEnvelope } from 'react-icons/fa'
import { FiUsers } from 'react-icons/fi'
import { GiBrain, GiScales, GiTeacher } from "react-icons/gi"

export default function About() {
  return (
    <div className="py-16">
      <div className="relative h-64 mb-16 overflow-hidden">
        <div className="absolute inset-0 bg-gradient-to-r from-primary-light via-blue-500 to-purple-600"></div>
        <div className="absolute inset-0 opacity-20 bg-[url('/grid-pattern.svg')]"></div>
        <div className="container-custom h-full flex items-center relative z-10">
          <div className="max-w-3xl">
            <h1 className="heading-1 text-white mb-4 font-heading">About Us</h1>
            <p className="text-xl text-white/90 leading-relaxed">
              Discover the beating heart of Neuron 5
            </p>
          </div>
        </div>
      </div>

      <div className="container-custom">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-12 lg:gap-20 mb-24">
          <div className="relative flex flex-col h-full">
            <div className="absolute top-0 left-0 w-16 h-1 bg-primary-light dark:bg-primary-light"></div>
            <h2 className="heading-2 text-gray-900 dark:text-white pt-6 mb-8 font-heading">Mission</h2>
            <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden mb-8 flex flex-grow">
              <div className="md:flex w-full h-full">
                <div className="p-8 flex-grow">
                  <div className="prose dark:prose-invert max-w-none">
                    <p className="text-gray-600 dark:text-gray-300">
                      Neuron 5 aims to provide top-quality services while ensuring long-term success leveraging proven business practices and ethical business principles whilst adopting the new AI technologies into the future.
                    </p>
                    <p className="text-gray-600 dark:text-gray-300">
                      We aim to solidify our position as a trusted and reputable partner in the industry by delivering innovative projects and data-driven insights.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div className="relative flex flex-col h-full">
            <div className="absolute top-0 left-0 w-16 h-1 bg-primary-light dark:bg-primary-light"></div>
            <h2 className="heading-2 text-gray-900 dark:text-white pt-6 mb-8 font-heading">Vision</h2>
            <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden mb-8 flex flex-grow">
              <div className="md:flex w-full h-full">
                <div className="p-8 flex-grow">
                  <div className="prose dark:prose-invert max-w-none">
                    <p className="text-gray-600 dark:text-gray-300">
                      Neuron 5 was founded with a clear vision to establish a sustainable business that can grow to meet all future project needs and become a fully fledged consultancy in its own right with year on year growth.
                    </p>
                    <p className="text-gray-600 dark:text-gray-300">
                      With 25 years of experience our broad range of knowledge and skills are valuable assets which will support the growth of the business.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>



          <div className="relative flex flex-col h-full">
            <div className="absolute top-0 left-0 w-16 h-1 bg-primary-light dark:bg-primary-light"></div>
            <h2 className="heading-2 text-gray-900 dark:text-white pt-6 mb-8 font-heading">Leadership</h2>
            <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden mb-8 flex flex-grow">
              <div className="md:flex w-full h-full">
                <div className="md:flex-shrink-0 md:w-1/3 bg-blue-100 dark:bg-blue-900 md:h-auto h-60 flex items-center justify-center relative">
                  <Image 
                    src="/images/founder.jpg" 
                    alt="Dan Harvey - Neuron 5 founder" 
                    fill
                    className="object-cover rounded-none md:rounded-l-xl"
                    priority // Optional: eager loading for above-the-fold images
                  />
                </div>
                <div className="p-8 flex-grow">
                  <h3 className="text-2xl font-semibold text-gray-900 dark:text-white mb-2 font-heading">Dan Harvey</h3>
                  <p className="uppercase tracking-wide text-sm text-primary-light dark:text-primary-light font-semibold mb-4">
                    Founder & Data / AI Engineer
                  </p>
                  <div className="prose dark:prose-invert max-w-none">
                    <p className="text-gray-600 dark:text-gray-300">
                      Dan is a huge rugby fan who enjoys attending international matches wherever possible.
                      He likes good food, travelling and has a great fondness for all things oceanic.
                    </p>
                  </div>
                  <div className="mb-4">
                    <Link 
                      href="https://linkedin.com/in/danxharvey" 
                      target="_blank"
                      rel="noopener noreferrer" 
                      className="inline-flex items-center text-primary-light dark:text-primary-light hover:underline"
                    >
                      <FaLinkedin className="mr-2" /> Full profile on LinkedIn
                    </Link>
                    <Link 
                      href="mailto:dan@neuron5.co.uk" 
                      className="inline-flex items-center text-primary-light dark:text-primary-light hover:underline"
                    >
                      <FaEnvelope className="mr-2" />dan@neuron5.co.uk
                    </Link>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div className="relative flex flex-col h-full">
            <div className="absolute top-0 left-0 w-16 h-1 bg-primary-light dark:bg-primary-light"></div>
            <h2 className="heading-2 text-gray-900 dark:text-white pt-6 mb-8 font-heading">Background</h2>

            <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden mb-8 flex flex-grow">
              <div className="md:flex w-full h-full">
                <div className="p-8 flex-grow">
                  <div className="prose dark:prose-invert max-w-none">
                    <p className="text-gray-600 dark:text-gray-300">
                      Dan's career began as a university intern with HSBC where he delivered bespoke software solutions across multiple call centre sites within the UK and Asia. His talent was quickly recognised and he was promoted into the bank's Group IT function as a frontend design analyst.
                    </p>
                    <p className="text-gray-600 dark:text-gray-300">
                      From here his journey really began as he freelanced across Europe and Australia, initially as a technical analyst before finding his passion in data with roles ranging from database design to business intelligence, whilst specialising in all things SQL, optimisation and reporting tools.
                      He used the lockdown period to complete his Masters.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
        
        <div className="bg-primary-light dark:bg-primary-dark bg-opacity-10 dark:bg-opacity-20 border-l-4 border-primary-light dark:border-primary-light p-8 rounded-r-xl mb-24">
          <div className="max-w-4xl mx-auto">
            <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-4 font-heading">Research Excellence</h3>
            <p className="text-gray-600 dark:text-gray-300 leading-relaxed">
              Dan finished as runner-up in the Research Prize within the faculty of Mathematics & Computer Science 
              where he developed an exciting proof of concept virtual assistant to assist blind people in locating objects. 
              Utilizing NLP and Computer Vision techniques allowed him to graduate with a distinction in Artificial Intelligence
              before joining a publishing research team.
            </p>
          </div>
        </div>
        
        <div className="mb-24">
          <div className="text-center mb-16">
            <div className="inline-block w-20 h-1 bg-primary-light dark:bg-primary-light mb-4"></div>
            <h2 className="heading-2 text-gray-900 dark:text-white font-heading">Core Neurons</h2>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8">
            <div className="bg-white dark:bg-gray-900 rounded-xl p-8 shadow-md hover:shadow-lg transition-shadow">
              <div className="text-primary-light dark:text-primary-light mb-4 flex justify-center">
                <GiBrain className="w-10 h-10" />
              </div>
              <h3 className="text-xl font-semibold mb-4 font-heading text-gray-900 dark:text-white flex justify-center">Critical Thinking</h3>
            </div>
            
            <div className="bg-white dark:bg-gray-900 rounded-xl p-8 shadow-md hover:shadow-lg transition-shadow">
              <div className="text-primary-light dark:text-primary-light mb-4 flex justify-center">
                <GiTeacher className="w-10 h-10" />
              </div>
              <h3 className="text-xl font-semibold mb-4 font-heading text-gray-900 dark:text-white flex justify-center">Lifelong Learning</h3>
            </div>
            
            <div className="bg-white dark:bg-gray-900 rounded-xl p-8 shadow-md hover:shadow-lg transition-shadow">
              <div className="text-primary-light dark:text-primary-light mb-4 flex justify-center">
                <GiScales className="w-10 h-10" />
              </div>
              <h3 className="text-xl font-semibold mb-4 font-heading text-gray-900 dark:text-white flex justify-center">Pragmatism</h3>
            </div>
            
            <div className="bg-white dark:bg-gray-900 rounded-xl p-8 shadow-md hover:shadow-lg transition-shadow">
              <div className="text-primary-light dark:text-primary-light mb-4 flex justify-center">
                <FiUsers className="w-10 h-10" />
              </div>
              <h3 className="text-xl font-semibold mb-4 font-heading text-gray-900 dark:text-white flex justify-center">Collaboration</h3>
            </div>
          </div>
        </div>

        <CTA2 />        

      </div>
    </div>
  )
}