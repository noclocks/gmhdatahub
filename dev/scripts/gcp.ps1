# GCP Workload Identity Pool and Service Account Setup
# https://github.com/google-github-actions/auth?tab=readme-ov-file#preferred-direct-workload-identity-federation

# Variables
$GITHUB_ORG = "noclocks"
$REPO_NAME = "gmhdatahub"
$PROJECT_ID = "gmh-communities"
$SERVICE = "gmhdatahub"
$REGION = "us-east1"

# Authenticate and configure gcloud
Write-Host "Authenticating with GCP..."
gcloud auth login
gcloud auth application-default login
gcloud auth application-default set-quota-project $PROJECT_ID
gcloud config set project $PROJECT_ID

# Get project number
Write-Host "Getting project number..."
$PROJECT_NUMBER = $(gcloud projects describe $PROJECT_ID --format="value(projectNumber)")

# Create workload identity pool
Write-Host "Creating workload identity pool..."
gcloud iam workload-identity-pools create "github" `
    --project=$PROJECT_ID `
    --location="global" `
    --display-name="GitHub Actions Pool"

# Get workload identity pool ID
Write-Host "Getting pool ID..."
$POOL_ID = $(gcloud iam workload-identity-pools describe "github" `
    --project=$PROJECT_ID `
    --location="global" `
    --format="value(name)")
Write-Host "Pool ID: $POOL_ID"

# Create workload identity pool provider
Write-Host "Creating workload identity pool provider..."
gcloud iam workload-identity-pools providers create-oidc "github" `
    --project=$PROJECT_ID `
    --location="global" `
    --workload-identity-pool="github" `
    --display-name="GitHub Repo Provider" `
    --attribute-mapping="google.subject=assertion.sub,attribute.actor=assertion.actor,attribute.repository=assertion.repository,attribute.repository_owner=assertion.repository_owner" `
    --attribute-condition="assertion.repository_owner == '$GITHUB_ORG'" `
    --issuer-uri="https://token.actions.githubusercontent.com"

# Get provider name
Write-Host "Getting provider name..."
$PROVIDER_NAME = $(gcloud iam workload-identity-pools providers describe "github" `
    --project=$PROJECT_ID `
    --location="global" `
    --workload-identity-pool="github" `
    --format="value(name)")
Write-Host "Provider Name: $PROVIDER_NAME"

# Create service account
Write-Host "Creating service account..."
gcloud iam service-accounts create "github-actions-sa" `
    --project=$PROJECT_ID `
    --display-name="GitHub Actions Service Account"

# Grant IAM roles
Write-Host "Granting IAM roles..."

# Artifact Registry Writer
gcloud projects add-iam-policy-binding $PROJECT_ID `
    --member="serviceAccount:github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
    --role="roles/artifactregistry.writer"

# Artifact Registry Reader
gcloud projects add-iam-policy-binding $PROJECT_ID `
  --member="serviceAccount:github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
  --role="roles/artifactregistry.reader"

# Workload Identity User
gcloud iam service-accounts add-iam-policy-binding `
    "github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
    --project=$PROJECT_ID `
    --member="principalSet://iam.googleapis.com/${POOL_ID}/attribute.repository/${GITHUB_ORG}/${REPO_NAME}" `
    --role="roles/iam.workloadIdentityUser"

# Cloud Run Admin
gcloud projects add-iam-policy-binding $PROJECT_ID `
    --member="serviceAccount:github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
    --role="roles/run.admin"

# service account user role
gcloud iam service-accounts add-iam-policy-binding `
  "github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
  --member="serviceAccount:github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
  --role="roles/iam.serviceAccountUser"

# Compute Engine Default Service Account
gcloud iam service-accounts add-iam-policy-binding `
  "${PROJECT_NUMBER}-compute@developer.gserviceaccount.com" `
  --member="serviceAccount:github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
  --role="roles/iam.serviceAccountUser"

# Service Account Token Creator for Cloud Run Service Agent
gcloud iam service-accounts add-iam-policy-binding `
    "${PROJECT_NUMBER}-compute@developer.gserviceaccount.com" `
    --member="serviceAccount:service-${PROJECT_NUMBER}@serverless-robot-prod.iam.gserviceaccount.com" `
    --role="roles/iam.serviceAccountTokenCreator"

# Create Artifact Registry
Write-Host "Creating Artifact Registry..."
gcloud artifacts repositories create $SERVICE `
    --repository-format=docker `
    --location=$REGION `
    --description="Docker repository for the GMH DataHub" `
    --project=$PROJECT_ID

Write-Host "Setup completed successfully!"
