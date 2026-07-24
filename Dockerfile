# Use the official slim Python image
FROM python:3.12-slim

# Prevent Python from writing .pyc files and buffer logs
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

# Install required packages
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    wget \
    unzip \
    ca-certificates \
    minizinc \
    swi-prolog \
    # Clean up
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Create a non-root user
RUN useradd --create-home --shell /bin/bash appuser

# Install Python dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy the application (excluding ignored files in .dockerignore)
COPY . .

# Give ownership to the non-root user
RUN chown -R appuser:appuser /app

# Switch to non-root user
USER appuser

EXPOSE 5000
CMD ["gunicorn", "--bind=0.0.0.0:5000", "--workers=4", "--timeout=0", "app:app"]
