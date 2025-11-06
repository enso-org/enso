package org.enso.base;

import org.apache.commons.mail.SimpleEmail;
import org.apache.commons.mail.Email;
import org.apache.commons.mail.EmailException;
import org.apache.commons.mail.DefaultAuthenticator;

public class SMTP_Email {
    public static void send(
            String smtpHost,
            int smtpPort,
            String username,
            String password,
            boolean enableTLS,
            String from,
            String[] to,
            String subject,
            String body) {
        try {
            Email email = new SimpleEmail();
            email.setHostName(smtpHost);
            email.setSmtpPort(smtpPort);
            email.setAuthenticator(new DefaultAuthenticator(username, password));

            // Configure SSL/TLS based on port and settings
            if (smtpPort == 465) {
                // Port 465 uses implicit SSL
                email.setSSLOnConnect(true);
            } else if (smtpPort == 587 && enableTLS) {
                // Port 587 uses STARTTLS
                email.setStartTLSEnabled(true);
                email.setStartTLSRequired(true);
            } else if (enableTLS) {
                // Other ports with TLS
                email.setSSLOnConnect(true);
            }

            email.setFrom(from);
            email.setSubject(subject);
            email.setMsg(body);
            for (String recipient : to) {
                email.addTo(recipient);
            }

            email.send();
            System.out.println("Email sent successfully to: " + to);
        } catch (EmailException e) {
            System.err.println("Failed to send email: " + e.getMessage());
            // Wrap checked exception to avoid leaking EmailException in the signature.
            throw new RuntimeException("Failed to send email", e);
        }
    }
}
