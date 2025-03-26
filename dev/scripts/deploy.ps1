

# Variables
$PROJECT_ID = "gmh-communities"
$SERVICE = "gmh-leasing-dashboard-dev" # "gmh-leasing-dashboard-prod" "gmhdatahub"
$REPO = "leasing-dashboard"
$REGION = "us-east1"
$TAG = "latest"

# Authenticate and configure gcloud
Write-Host "Authenticating with GCP..."
gcloud auth login
gcloud auth application-default login
gcloud auth application-default set-quota-project $PROJECT_ID
gcloud config set project $PROJECT_ID

# Authenticate for docker
gcloud auth configure-docker "$REGION-docker.pkg.dev"

$DOCKER_TAG = "$REGION-docker.pkg.dev/$PROJECT_ID/$REPO/$SERVICE" + ":${TAG}"

# docker commands
docker build --build-arg NOCLOCKS_ENCRYPTION_KEY=$ENV:NOCLOCKS_ENCRYPTION_KEY -t gmhdatahub:latest .
$DOCKER_TAG = "$REGION-docker.pkg.dev/$PROJECT_ID/$SERVICE/$SERVICE" + ":${TAG}"
docker tag gmh/gmhdatahub:latest $DOCKER_TAG
docker push $DOCKER_TAG

# deploy
gcloud run deploy $SERVICE `
      --image=$DOCKER_TAG `
      --region=$REGION `
      --allow-unauthenticated `
      --set-env-vars=R_CONFIG_ACTIVE=default `
      --set-env-vars='R_CONFIG_FILE=/secrets/config.yml' `
      --set-cloudsql-instances=gmh-communities:us-east1:gmh-cloudsql-dev `
      --update-secrets="$secretRef" `
      --project=$PROJECT_ID

gcloud run services update-traffic $SERVICE --to-latest --region=$REGION --project=$PROJECT_ID
