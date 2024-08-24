# Use a Rust base image
FROM rust:1.76.0 as builder

# Install wasm-pack
RUN curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Set the working directory
WORKDIR /usr/src/numbat

# Copy the entire project
COPY . .

# Build the WebAssembly package
RUN cd numbat-wasm && wasm-pack build --target web --out-dir www/pkg

# Use a lightweight web server as the final image
FROM nginx:alpine

# Copy the built files from the builder stage
COPY --from=builder /usr/src/numbat/numbat-wasm/www /usr/share/nginx/html

# Expose port 8192
EXPOSE 8192

# The default command starts Nginx
CMD ["nginx", "-g", "daemon off;"]