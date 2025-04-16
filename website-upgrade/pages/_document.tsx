// pages/_document.tsx
import Document, { Html, Head, Main, NextScript } from 'next/document'

class MyDocument extends Document {
  render() {
    return (
      <Html>
        <Head>
          {/* Theme detection script */}
          <script
            dangerouslySetInnerHTML={{
              __html: `
                (function() {
                  // Check if user has a saved theme preference in localStorage
                  const savedTheme = localStorage.getItem('theme');
                  if (savedTheme) {
                    document.documentElement.classList.add(savedTheme);
                  } else {
                    // Fallback to the prefers-color-scheme
                    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
                    document.documentElement.classList.add(prefersDark ? 'dark' : 'light');
                  }
                })();
              `
            }}
          />
        </Head>
        <body>
          <Main />
          <NextScript />
        </body>
      </Html>
    )
  }
}

export default MyDocument
