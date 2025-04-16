'use client'

import { useState, useEffect } from 'react';
import Link from 'next/link';
import Image from 'next/image';
import { useTheme } from 'next-themes';

const HeroSection = () => {
  const [mounted, setMounted] = useState(false);
  const { theme } = useTheme();

  useEffect(() => {
    setMounted(true);
  }, []);

  // Use a fallback image while the component is mounting
  const imageSrc = mounted && theme === 'dark' ? '/images/neuron5black.png' : '/images/neuron5.png';

  return (
    <section className="py-8 lg:py-12">
      <div className="container-custom">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-12">
          {/* Image Column - appears first on mobile */}
          <div className="flex justify-center items-center order-1 lg:order-2">
            <div className="relative w-64 h-64 lg:w-80 lg:h-80 rounded-full bg-black dark:bg-white overflow-hidden">
              {mounted ? (
                <Image
                  src={imageSrc}
                  alt="Neuron 5"
                  priority
                  fill
                  className="object-cover"
                />
              ) : (
                <div className="absolute inset-0 flex items-center justify-center">
                  <span className="text-primary-light dark:text-primary-dark font-bold text-xl">NEURON 5</span>
                </div>
              )}
            </div>
          </div>
          
          {/* Text Column - appears second on mobile */}
          <div className="space-y-8 order-2 lg:order-1 flex flex-col justify-center items-center lg:items-start">
            <h1 className="heading-1 text-gray-900 dark:text-white">
              <span className="text-primary-light dark:text-primary-dark block">
                Innovative Solutions
              </span>
              Complex Challenges
            </h1>
            <p className="text-lg text-gray-600 dark:text-gray-300 text-center lg:text-left">
              Simplifying complexity with intelligent automation to solve
              your toughest problems and push the boundaries of what's possible.
            </p>
            <div>
              <Link href="/about" className="btn-primary">
                Learn More
              </Link>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};

export default HeroSection;
