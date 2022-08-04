#!/bin/sh

pghostname=postgrestest.enso.org
pgipaddress=127.0.0.1

cat > csr.conf <<EOF
[ req ]
default_bits = 2048
prompt = no
default_md = sha256
req_extensions = req_ext
distinguished_name = dn

[ dn ]
C = UK
ST = London
L = London
O = Enso
OU = Enso Dev
CN = $pghostname

[ req_ext ]
subjectAltName = @alt_names

[ alt_names ]
DNS.1 = $pghostname
IP.1 = $pgipaddress
EOF

cat > cert.conf <<EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage = digitalSignature, nonRepudiation, keyEncipherment, dataEncipherment
subjectAltName = @alt_names

[alt_names]
DNS.1 = $pghostname
DNS.2 = $pgipaddress
EOF

docker build --build-arg host_name=${pghostname} -t postgres-ssl .
docker run -d --name postgres-ssl -p 5432:5432 -e POSTGRES_PASSWORD=$ENSO_DATABASE_TEST_DB_PASSWORD postgres-ssl
docker cp postgres-ssl:/openssl/rootCA.crt ../../data/transient/rootCA.crt

rm csr.conf
rm cert.conf
