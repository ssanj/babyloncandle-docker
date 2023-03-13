FROM arm64v8/haskell:9.0.2

WORKDIR /app

ADD blog-src ./

EXPOSE 9999

# Compile and copy binaries to /usr/.local/bin
RUN stack install

# Install rsync for copying over dist to website
RUN apt update && apt install rsync -y

ENTRYPOINT [ "/root/.local/bin/site", "watch" ]
