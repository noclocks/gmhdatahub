
# Variables
$REPO_NAME = "gmhdatahub"
$PROJECT_ID = "gmh-communities"
$SERVICE = "gmhdatahub"
$REGION = "us-east1"

$APIs = @(
  "addressvalidation.googleapis.com",
  "aiplatform.googleapis.com",
  "analyticshub.googleapis.com",
  "appengineflex.googleapis.com",
  "artifactregistry.googleapis.com",
  "bigquery.googleapis.com",
  "bigqueryconnection.googleapis.com",
  "bigquerydatapolicy.googleapis.com",
  "bigquerymigration.googleapis.com",
  "bigqueryreservation.googleapis.com",
  "bigquerystorage.googleapis.com",
  "cloudaicompanion.googleapis.com",
  "cloudapis.googleapis.com",
  "cloudasset.googleapis.com",
  "cloudbuild.googleapis.com",
  "cloudresourcemanager.googleapis.com",
  "cloudtrace.googleapis.com",
  "compute.googleapis.com",
  "containerregistry.googleapis.com",
  "dataform.googleapis.com",
  "dataplex.googleapis.com",
  "datastore.googleapis.com",
  "deploymentmanager.googleapis.com",
  "directions-backend.googleapis.com",
  "distance-matrix-backend.googleapis.com",
  "elevation-backend.googleapis.com",
  "geocoding-backend.googleapis.com",
  "geolocation.googleapis.com",
  "iam.googleapis.com",
  "iamcredentials.googleapis.com",
  "logging.googleapis.com",
  "maps-android-backend.googleapis.com",
  "maps-backend.googleapis.com",
  "maps-embed-backend.googleapis.com",
  "maps-ios-backend.googleapis.com",
  "mapsplatformdatasets.googleapis.com",
  "monitoring.googleapis.com",
  "networkmanagement.googleapis.com",
  "oslogin.googleapis.com",
  "parametermanager.googleapis.com",
  "places-backend.googleapis.com",
  "places.googleapis.com",
  "pubsub.googleapis.com",
  "roads.googleapis.com",
  "run.googleapis.com",
  "secretmanager.googleapis.com",
  "servicemanagement.googleapis.com",
  "servicenetworking.googleapis.com",
  "serviceusage.googleapis.com",
  "sql-component.googleapis.com",
  "sqladmin.googleapis.com",
  "static-maps-backend.googleapis.com",
  "storage-api.googleapis.com",
  "storage-component.googleapis.com",
  "storage.googleapis.com",
  "storageinsights.googleapis.com",
  "street-view-image-backend.googleapis.com",
  "streetviewpublish.googleapis.com",
  "timezone-backend.googleapis.com"
)

# Enable APIs
foreach ($api in $APIs) {
  Write-Host "Enabling API: $api"
  gcloud services enable $api --project=$PROJECT_ID
}

# IAM Policy Binding
gcloud projects add-iam-policy-binding $PROJECT_ID --member="user:jimmy.briggs@noclocks.dev" --role=roles/cloudaicompanion.user

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
# Service account user role
gcloud iam service-accounts add-iam-policy-binding `
  "github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
  --member="serviceAccount:github-actions-sa@${PROJECT_ID}.iam.gserviceaccount.com" `
  --role="roles/iam.serviceAccountUser"
# Compute Engine Default Service Account
gcloud iam service-accounts add-iam-policy-binding `
  "${}
