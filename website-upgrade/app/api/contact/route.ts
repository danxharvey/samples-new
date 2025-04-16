import { Resend } from 'resend'
import { NextResponse } from 'next/server'

const resend = new Resend(process.env.RESEND_API_KEY)

export async function POST(req: Request) {
  try {
    const { name, email, subject, message } = await req.json()

    // Simple validation: Make sure all fields are provided
    if (!name || !email || !message) {
      return NextResponse.json({ success: false, error: 'All fields are required' }, { status: 400 })
    }

    const data = await resend.emails.send({
      from: 'Webform <hello@neuron5.co.uk>',
      to: ['hello@neuron5.co.uk'],
      subject: subject || 'New Contact Form Submission',
      html: `<strong>Name:</strong> ${name}<br/>
             <strong>Email:</strong> ${email}<br/>
             <strong>Message:</strong><br/>${message.replace(/\n/g, '<br/>')}`
    })

    return NextResponse.json({ success: true, data })
  } catch (error) {
    console.error('Resend error:', error)
    return NextResponse.json({ success: false, error: 'Internal Server Error' }, { status: 500 })
  }
}
