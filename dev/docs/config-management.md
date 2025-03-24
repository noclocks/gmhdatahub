# GMH Leasing Dashboard Configuration Management

This document describes the approach used for managing configuration in the GMH Leasing Dashboard application, including how configuration is handled in different environments.

## Overview

The application uses a `config.yml` file for configuration with the following capabilities:

1. **Development Environment**: Uses a local `config.yml` file
2. **Docker Development**: Mounts the configuration as a volume
3. **Cloud Run Production**: Uses GCP Secret Manager to securely store and access the configuration

## Configuration Loading

The application looks for `config.yml` in the following locations (in priority order):

1. GCP Secret mounted at `/secrets/config.yml` (production)
2. Container path at `/etc/gmhdatahub/config.yml` (Docker)
3. Working directory (local development)
4. Package directory (fallback)

## Local Development

During local development, simply place a `config.yml` file in your working directory. The application's `.onLoad` function will automatically detect and use it.

## Docker Testing

For testing with Docker locally, use the provided scripts:

### Running a Local Docker Container

```powershell
# Navigate to the project root (where config.yml is located)
cd /path/to/project

# Run the local Docker script
./dev/scripts/run-local-docker.ps1
```

This script:
- Builds a Docker image using `Dockerfile.prelease`
- Mounts your local `config.yml` to `/secrets/config.yml` in the container
- Exposes the application on port 8080

### Cleaning Up Local Docker Environment

```powershell
# Clean up the Docker container and optionally the image
./dev/scripts/cleanup-local-docker.ps1
```

## Deployment to Cloud Run

For deploying to Cloud Run with the configuration securely stored in GCP Secret Manager:

```powershell
# Deploy to Cloud Run
./dev/scripts/deploy-prelease.ps1
```

This script:
1. Authenticates with GCP
2. Creates/updates a secret in Secret Manager with your `config.yml` content
3. Builds the Docker image
4. Pushes the image to Google Artifact Registry
5. Deploys to Cloud Run with the secret mounted at `/secrets/config.yml`

## Updating Configuration Without Redeployment

To update the configuration in GCP Secret Manager without having to rebuild and redeploy the entire application:

```powershell
# Update the configuration in Secret Manager
./dev/scripts/update-config-secret.ps1
```

This script:
1. Authenticates with GCP
2. Verifies the secret exists in Secret Manager
3. Adds a new version of the secret with your updated `config.yml` content
4. Returns the latest version number

The changes will be picked up the next time the Cloud Run service restarts. To force an immediate update, you can manually restart the service after updating the secret.

## Security Considerations

- The `config.yml` file contains sensitive information and should never be committed to version control
- In the Docker container, the configuration is mounted as a read-only volume
- In Cloud Run, the configuration is stored in Secret Manager and mounted securely
- No encryption keys are needed in the container
- Config updates can be made by updating the secret in GCP Secret Manager without rebuilding the container

## Troubleshooting

### Verifying Configuration in a Docker Container

To check if the configuration file is properly mounted in a local Docker container:

```powershell
docker exec gmhdatahub-test ls -la /secrets/config.yml
```

### Viewing Container Logs

```powershell
docker logs gmhdatahub-test
```

### Accessing the Container Shell

```powershell
docker exec -it gmhdatahub-test bash
```
