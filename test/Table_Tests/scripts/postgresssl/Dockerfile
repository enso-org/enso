# syntax=docker/dockerfile:1
FROM postgres:14.4

ARG host_name

COPY ./*.conf /root/

RUN apt-get update && \
    apt-get upgrade -y && \
    mkdir openssl && cd openssl && \
    openssl req -x509 -sha256 -days 356 -nodes -newkey rsa:2048 -subj "/CN=${host_name}/C=UK/L=London" -keyout rootCA.key -out rootCA.crt && \
    openssl genrsa -out server.key 2048 && \
    openssl req -new -key server.key -out server.csr -config /root/csr.conf && \
    openssl x509 -req -in server.csr -CA rootCA.crt -CAkey rootCA.key -CAcreateserial -out server.crt -days 365 -sha256 -extfile /root/cert.conf && \
    chmod 400 server.key && \
    chmod 444 server.crt && \
    chown postgres:postgres server.*

CMD ["postgres", "-c", "ssl=on", "-c", "ssl_cert_file=/openssl/server.crt", "-c", "ssl_key_file=/openssl/server.key"]
