
$INSTANCE_ID = "gmh-cloudsql-prod"

gcloud sql instances patch $INSTANCE_ID --tier=db-custom-1-3840
