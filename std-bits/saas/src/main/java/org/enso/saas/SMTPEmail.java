package org.enso.saas;

import org.apache.commons.mail.SimpleEmail;
import org.apache.commons.mail.Email;
import org.apache.commons.mail.EmailException;
import org.apache.commons.mail.DefaultAuthenticator;

public class SMTPEmail {
    public static void send(
            String smtpHost,
            int smtpPort,
            String username,
            String password,
            boolean enableTLS,
            String from,
            String fromName,
            String[] to,
            String[] cc,
            String[] bcc,
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

            if (fromName != null && !fromName.isEmpty()) {
                email.setFrom(from, fromName);
            } else {
                email.setFrom(from);
            }
            email.setSubject(subject);
            email.setMsg(body);
            if (to != null) {
                for (String recipient : to) {
                    email.addTo(recipient);
                }
            }
            if (cc != null) {
                for (String recipient : cc) {
                    email.addCc(recipient);
                }
            }
            if (bcc != null) {
                for (String recipient : bcc) {
                    email.addBcc(recipient);
                }
            }
            email.send();
        } catch (EmailException e) {
            System.err.println("Failed to send email: " + e.getMessage());
            // Wrap checked exception to avoid leaking EmailException in the signature.
            throw new RuntimeException("Failed to send email", e);
        }
    }
}
