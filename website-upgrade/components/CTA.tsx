'use client'

import Link from 'next/link'

export default function CTA() {
  return (
    <section className="py-16">
      <div className="container-custom">
        <div className="bg-primary-light dark:bg-primary-dark bg-opacity-10 dark:bg-opacity-20 rounded-2xl p-10 md:p-16">
          <div className="max-w-3xl mx-auto text-center space-y-8">
            <h2 className="heading-2 text-gray-900 dark:text-white">
              Ready to transform your data into intelligence?
            </h2>
            <p className="text-lg text-gray-600 dark:text-gray-300">
              Let's discuss how our AI and data solutions can help your organisation thrive.
            </p>
            <div>
              <Link href="/contact" className="btn-primary">
                Contact Us
              </Link>
            </div>
          </div>
        </div>
      </div>
    </section>
  )
}
