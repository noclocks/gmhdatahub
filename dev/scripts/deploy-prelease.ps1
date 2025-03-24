####################################
# GMH Leasing Dashboard Deployment Script
# This script handles the deployment of the GMH Leasing Dashboard to GCP Cloud Run
# with configuration stored in Secret Manager
####################################

# Variables
$PROJECT_ID = "gmh-communities"
$SERVICE = "gmh-leasing-dashboard-prod"
$REPO = "leasing-dashboard"
$REGION = "us-east1"
$TAG = "latest"
$SECRET_NAME = "gmh-leasing-config"

# Set error action preference to continue so the script doesn't stop on warnings
$ErrorActionPreference = "Continue"

try {
    ####################################
    # STEP 1: Authenticate with GCP
    ####################################
    Write-Host "`n========== AUTHENTICATING WITH GCP ==========`n" -ForegroundColor Cyan

    # Suppress the warning messages from the Google Cloud SDK's PowerShell scripts
    $originalErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = "SilentlyContinue"

    gcloud auth login
    gcloud auth application-default login
    gcloud auth application-default set-quota-project $PROJECT_ID
    gcloud config set project $PROJECT_ID

    # Authenticate for docker
    gcloud auth configure-docker "$REGION-docker.pkg.dev"

    # Restore error action preference
    $ErrorActionPreference = $originalErrorActionPreference

    ####################################
    # STEP 2: Manage Secret in Secret Manager
    ####################################
    Write-Host "`n========== MANAGING SECRETS ==========`n" -ForegroundColor Cyan

    # Check if config.yml exists
    if (-not (Test-Path "config.yml")) {
        throw "config.yml file not found in the current directory"
    }

    # Check if secret exists
    $secretExists = $false
    try {
        $secretInfo = gcloud secrets describe $SECRET_NAME --format="value(name)" 2>$null
        if ($secretInfo) {
            $secretExists = $true
            Write-Host "Secret $SECRET_NAME already exists. Adding new version." -ForegroundColor Green
        }
    } catch {
        Write-Host "Secret $SECRET_NAME does not exist. Will create it." -ForegroundColor Yellow
    }

    if (-not $secretExists) {
        # Create new secret
        Write-Host "Creating new secret: $SECRET_NAME" -ForegroundColor Yellow
        gcloud secrets create $SECRET_NAME --replication-policy="automatic"
        if ($LASTEXITCODE -ne 0) {
            throw "Failed to create secret $SECRET_NAME"
        }
    }

    # Add new version with config.yml content
    Write-Host "Adding new version to secret: $SECRET_NAME" -ForegroundColor Yellow
    gcloud secrets versions add $SECRET_NAME --data-file="config.yml"
    if ($LASTEXITCODE -ne 0) {
        throw "Failed to add new version to secret $SECRET_NAME"
    }
    Write-Host "Secret successfully updated" -ForegroundColor Green

    ####################################
    # STEP 3: Build and Push Docker Image
    ####################################
    Write-Host "`n========== BUILDING AND PUSHING DOCKER IMAGE ==========`n" -ForegroundColor Cyan

    # derive tag
    $DOCKER_TAG = "$REGION-docker.pkg.dev/$PROJECT_ID/$REPO/$SERVICE" + ":${TAG}"

    # docker build
    Write-Host "Building Docker image: $DOCKER_TAG" -ForegroundColor Yellow
    docker build --no-cache --file .\Dockerfile.prelease -t $DOCKER_TAG .
    if ($LASTEXITCODE -ne 0) {
        throw "Docker build failed"
    }

    # docker push
    Write-Host "Pushing Docker image to Google Artifact Registry..." -ForegroundColor Yellow
    docker push $DOCKER_TAG
    if ($LASTEXITCODE -ne 0) {
        throw "Docker push failed"
    }
    Write-Host "Docker image built and pushed successfully" -ForegroundColor Green

    ####################################
    # STEP 4: Deploy to Cloud Run
    ####################################
    Write-Host "`n========== DEPLOYING TO CLOUD RUN ==========`n" -ForegroundColor Cyan

    # Construct the secret reference for Cloud Run
    # Format: mount-path=secret-name:secret-version
    $secretRef = "/secrets/config.yml=$SECRET_NAME:latest"

    Write-Host "Secret reference: $secretRef" -ForegroundColor Yellow
    Write-Host "Deploying to Cloud Run..." -ForegroundColor Yellow

    # Deploy to Cloud Run with mounted secret
    gcloud run deploy $SERVICE `
      --image=$DOCKER_TAG `
      --region=$REGION `
      --allow-unauthenticated `
      --update-secrets="$secretRef"

    if ($LASTEXITCODE -ne 0) {
        throw "Deployment to Cloud Run failed"
    }

    Write-Host "`n========== DEPLOYMENT COMPLETED SUCCESSFULLY ==========`n" -ForegroundColor Green
    Write-Host "GMH Leasing Dashboard has been deployed to Cloud Run" -ForegroundColor Green
    Write-Host "Configuration is securely stored in Secret Manager" -ForegroundColor Green
    Write-Host "Service URL: https://$SERVICE-<hash>.a.run.app" -ForegroundColor Green

} catch {
    Write-Host "`n========== DEPLOYMENT FAILED ==========`n" -ForegroundColor Red
    Write-Host "Error: $_" -ForegroundColor Red
    Write-Host "Stack Trace: $($_.ScriptStackTrace)" -ForegroundColor Red
    exit 1
}
