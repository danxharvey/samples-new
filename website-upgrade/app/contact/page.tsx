'use client'

import { useState } from 'react'
import { FiMapPin, FiMail, FiPhone, FiSend, FiLinkedin } from 'react-icons/fi'

const useContactForm = () => {
  const [formState, setFormState] = useState({
    name: '',
    email: '',
    subject: '',
    message: ''
  })
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [submitted, setSubmitted] = useState(false)
  const [error, setError] = useState('')

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    setFormState({
      ...formState,
      [e.target.name]: e.target.value
    })
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()

    if (!formState.name || !formState.email || !formState.message) {
      setError('Please fill in all fields.')
      return
    }

    setIsSubmitting(true)
    setError('')
    setSubmitted(false)

    try {
      const res = await fetch('/api/contact', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(formState)
      })

      const result = await res.json()

      if (result.success) {
        setSubmitted(true)
        setFormState({ name: '', email: '', subject: '', message: '' })

        setTimeout(() => {
          setSubmitted(false)
        }, 3000)
      } else {
        setError(result.error || 'Something went wrong, please try again.')
      }
    } catch (err) {
      setError('An error occurred. Please try again later.')
    }

    setIsSubmitting(false)
  }

  return { formState, isSubmitting, submitted, error, handleChange, handleSubmit }
}

export default function Contact() {
  const { formState, isSubmitting, submitted, error, handleChange, handleSubmit } = useContactForm()

  return (
    <div className="py-16">
      {/* Hero section with gradient background */}
      <div className="relative h-64 mb-16 overflow-hidden">
        <div className="absolute inset-0 bg-gradient-to-r from-primary-light via-blue-500 to-purple-600"></div>
        <div className="absolute inset-0 opacity-20 bg-[url('/grid-pattern.svg')]"></div>
        <div className="container-custom h-full flex items-center relative z-10">
          <div className="max-w-2xl">
            <h1 className="heading-1 text-white mb-4 font-heading">Get in Touch</h1>
            <p className="text-xl text-white/90 hidden md:block whitespace-nowrap">
              Let's discuss how our AI and data science expertise can transform your business.
            </p>
            <p className="text-xl text-white/90 md:hidden">
              Expert AI and data science solutions.
            </p>
          </div>
        </div>
      </div>
      
      <div className="container-custom">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
          {/* Contact form */}
          <div>
            <div className="bg-white dark:bg-gray-900 rounded-xl shadow-xl overflow-hidden">
              <div className="p-8">
                <h2 className="heading-3 text-gray-900 dark:text-white mb-6 font-heading">Send Us a Message</h2>
                
                {submitted ? (
                  <div className="text-center py-10">
                    <div className="inline-flex items-center justify-center w-16 h-16 rounded-full bg-green-100 text-green-500 mb-4">
                      <FiSend className="w-8 h-8" />
                    </div>
                    <h3 className="text-xl font-semibold mb-2 font-heading">Message Sent!</h3>
                    <p className="text-gray-600 dark:text-gray-400">
                      We'll get back to you as soon as possible.
                    </p>
                  </div>
                ) : (
                  <form onSubmit={handleSubmit} className="space-y-6">
                    {error && <div className="text-red-500 text-center mb-4">{error}</div>}
                    
                    <div className="group">
                      <label htmlFor="name" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                        Name
                      </label>
                      <input
                        type="text"
                        id="name"
                        name="name"
                        value={formState.name}
                        onChange={handleChange}
                        className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-md focus:ring-2 focus:ring-primary-light dark:focus:ring-primary-light focus:border-transparent transition-all bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                        required
                      />
                    </div>
                    
                    <div className="group">
                      <label htmlFor="email" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                        Email
                      </label>
                      <input
                        type="email"
                        id="email"
                        name="email"
                        value={formState.email}
                        onChange={handleChange}
                        className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-md focus:ring-2 focus:ring-primary-light dark:focus:ring-primary-light focus:border-transparent transition-all bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                        required
                      />
                    </div>
                    
                    <div className="group">
                      <label htmlFor="subject" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                        Subject
                      </label>
                      <input
                        type="text"
                        id="subject"
                        name="subject"
                        value={formState.subject}
                        onChange={handleChange}
                        className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-md focus:ring-2 focus:ring-primary-light dark:focus:ring-primary-light focus:border-transparent transition-all bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                      />
                    </div>
                    
                    <div className="group">
                      <label htmlFor="message" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                        Message
                      </label>
                      <textarea
                        id="message"
                        name="message"
                        value={formState.message}
                        onChange={handleChange}
                        rows={5}
                        className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-md focus:ring-2 focus:ring-primary-light dark:focus:ring-primary-light focus:border-transparent transition-all bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                        required
                      ></textarea>
                    </div>
                    
                    <button
                      type="submit"
                      disabled={isSubmitting}
                      className={`w-full py-3 px-6 rounded-md text-white font-medium transition-all relative overflow-hidden ${isSubmitting ? 'bg-gray-400' : 'bg-primary-light dark:bg-primary-light hover:bg-opacity-90'}`}
                    >
                      {isSubmitting ? (
                        <span className="flex items-center justify-center">
                          <svg className="animate-spin -ml-1 mr-3 h-5 w-5 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                            <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
                            <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                          </svg>
                          Sending...
                        </span>
                      ) : "Send Message"}
                    </button>
                  </form>
                )}
              </div>
            </div>
          </div>

          {/* Contact information and map */}
          <div className="space-y-8">
            {/* Map placeholder */}
            <div className="rounded-xl overflow-hidden shadow-xl h-72 relative">
              <iframe 
                src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d17868.100592168614!2d-3.4042246117346305!3d55.98957915464253!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x4887cf46f48f455d%3A0x5be5c948e8bbdd77!2sSouth%20Queensferry%2C%20Queensferry%2C%20UK!5e0!3m2!1sen!2sus!4v1649341215618!5m2!1sen!2sus" 
                width="100%" 
                height="100%" 
                style={{ border: 0 }} 
                allowFullScreen 
                loading="lazy" 
                referrerPolicy="no-referrer-when-downgrade"
                className="absolute inset-0"
              ></iframe>
            </div>
            
            {/* Contact information cards */}
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div className="bg-white dark:bg-gray-900 p-6 rounded-xl shadow-md">
                <div className="inline-flex items-center justify-center w-12 h-12 rounded-full bg-primary-light bg-opacity-10 text-primary-light dark:text-primary-light mb-4">
                  <FiMapPin className="w-6 h-6"/>
                </div>
                <h3 className="text-lg font-semibold mb-2 font-heading text-center">Location</h3>
                <p className="text-gray-600 dark:text-gray-400 text-center">
                  South Queensferry<br />
                  Greater Edinburgh<br />
                  Scotland
                </p>
              </div>
              
              <div className="bg-white dark:bg-gray-900 p-6 rounded-xl shadow-md">
                <div className="inline-flex items-center justify-center w-12 h-12 rounded-full bg-primary-light bg-opacity-10 text-primary-light dark:text-primary-light mb-4">
                  <FiMail className="w-6 h-6" />
                </div>
                <h3 className="text-lg font-semibold mb-2 font-heading text-center">Email</h3>
                <a 
                  href="mailto:hello@neuron5.co.uk" 
                  className="text-primary-light dark:text-primary-light hover:underline block text-center"
                >
                  hello@neuron5.co.uk
                </a>
              </div>
              
              <div className="bg-white dark:bg-gray-900 p-6 rounded-xl shadow-md">
                <div className="inline-flex items-center justify-center w-12 h-12 rounded-full bg-primary-light bg-opacity-10 text-primary-light dark:text-primary-light mb-4">
                  <FiPhone className="w-6 h-6" />
                </div>
                <h3 className="text-lg font-semibold mb-2 font-heading text-center">Phone</h3>
                <p className="text-gray-600 dark:text-gray-400 text-center">
                  UK: +44 7899 702 453<br />
                  IRL: +353 86 056 9070
                </p>
              </div>
              
              <div className="bg-white dark:bg-gray-900 p-6 rounded-xl shadow-md">
                <div className="inline-flex items-center justify-center w-12 h-12 rounded-full bg-primary-light bg-opacity-10 text-primary-light dark:text-primary-light mb-4">
                  <FiLinkedin className="w-6 h-6" />
                </div>
                <h3 className="text-lg font-semibold mb-2 font-heading text-center">Connect</h3>
                <a 
                  href="https://linkedin.com/company/neuron5" 
                  target="_blank" 
                  rel="noopener noreferrer"
                  className="text-primary-light dark:text-primary-light hover:underline block text-center"
                >
                  LinkedIn
                </a>
              </div>
            </div>
          </div>
        </div>
      </div>
      
      {/* FAQ Section */}
      <div className="container-custom mt-24">
        <div>
          <h2 className="heading-2 text-center text-gray-900 dark:text-white mb-12 font-heading">Frequently Asked Questions</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {/* First Column */}
            <div className="space-y-6">
              <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden">
                <div className="p-6 flex-grow">
                  <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-3 font-heading">What types of projects do you specialise in?</h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    With over 25 years experience we can handle any project from analysis through to techical implementation including automation tasks.
                  </p>
                </div>
              </div>
              
              <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden">
                <div className="p-6 flex-grow">
                  <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-3 font-heading">How do you typically engage with clients?</h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    We offer consultancy services, project-based work, and ongoing support. Our engagement typically begins with a discovery call to understand your needs.
                  </p>
                </div>
              </div>
            </div>
            
            {/* Second Column */}
            <div className="space-y-6">
              <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden">
                <div className="p-6 flex-grow">
                  <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-3 font-heading">Do you work with startups or only enterprise clients?</h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    We work with organisations of all sizes, from startups to enterprise clients. Our solutions are tailored to match your specific needs and budget.
                  </p>
                </div>
              </div>
              
              <div className="bg-white dark:bg-gray-900 rounded-xl shadow-md overflow-hidden">
                <div className="p-6 flex-grow">
                  <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-3 font-heading">What is your typical project timeline?</h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Project timelines vary based on complexity and scope. We provide detailed timelines during our initial consultation phase.
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
