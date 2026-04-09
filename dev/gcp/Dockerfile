FROM gcr.io/google.com/cloudsdktool/google-cloud-cli:latest

# Avoid installing apt recommended packages to keep the dev image smaller and reduce unused dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        default-mysql-client \
    && rm -rf /var/lib/apt/lists/*
