export default function PageHeader({ title, description }: { title: string; description?: string }) {
  return (
    <div className="relative h-64 mb-16 overflow-hidden">
      <div className="absolute inset-0 bg-gradient-to-r from-primary-light via-blue-500 to-purple-600"></div>
      <div className="absolute inset-0 opacity-20 bg-[url('/grid-pattern.svg')]"></div>
      <div className="container-custom h-full flex items-center relative z-10">
        <div className="max-w-2xl">
          <h1 className="heading-1 text-white mb-4 font-heading">{title}</h1>
          {description && (
            <p className="text-xl text-white/90">{description}</p>
          )}
        </div>
      </div>
    </div>
  )
}
