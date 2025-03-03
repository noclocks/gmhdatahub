

# Variables
$PROJECT_ID = "gmh-communities"
$SERVICE = "gmh-leasing-dashboard-prod"
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

# derive tag
$DOCKER_TAG = "$REGION-docker.pkg.dev/$PROJECT_ID/$REPO/$SERVICE" + ":${TAG}"

# docker build
docker build --no-cache --build-arg NOCLOCKS_ENCRYPTION_KEY=$ENV:NOCLOCKS_ENCRYPTION_KEY --file .\Dockerfile.prelease -t $DOCKER_TAG .

# docker run
# docker run -it -p 8080:8080 -d $DOCKER_TAG

# docker push
docker push $DOCKER_TAG

# deploy
gcloud run deploy $SERVICE `
  --image=$DOCKER_TAG `
  --region=$REGION `
  --allow-unauthenticated
