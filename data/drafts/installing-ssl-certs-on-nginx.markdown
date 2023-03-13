Installing SSL

1. Generate a private key
2. Generate a certificate signing request
3. Supply the above CSR to your SSL provider
4. Use the chained certificate from your provider and your private key to enable SSL in nginx.

 - root ca
 - intermediate certs
 - application cert


 cert order
 -----------
 application cert > intermediate cert > root cert

     cat example.com.pem sub.class1.server.ca.pem ca.pem > example.com_chain.pem

a browser would check the application cert first, then it's parent cert and so on until
it finds the root certs.