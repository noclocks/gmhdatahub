####################################
# GMH Leasing Dashboard Local Docker Cleanup Script
# This script stops and removes the local Docker container and image
# used for testing purposes
####################################

# Variables
$IMAGE_NAME = "gmhdatahub-local"
$CONTAINER_NAME = "gmhdatahub-test"

# Set error action preference
$ErrorActionPreference = "Continue"

Write-Host "`n========== CLEANING UP LOCAL DOCKER ENVIRONMENT ==========`n" -ForegroundColor Cyan

# Step 1: Check if container exists and stop it
Write-Host "Checking for running container..." -ForegroundColor Yellow
$containerRunning = $false
try {
    $containerStatus = docker inspect --format="{{.State.Running}}" $CONTAINER_NAME 2>$null
    if ($containerStatus -eq "true") {
        $containerRunning = $true
        Write-Host "Container $CONTAINER_NAME is running. Stopping it..." -ForegroundColor Yellow
        docker stop $CONTAINER_NAME
        if ($LASTEXITCODE -eq 0) {
            Write-Host "Container stopped successfully." -ForegroundColor Green
        } else {
            Write-Host "Failed to stop container. It may already be stopped." -ForegroundColor Red
        }
    } else {
        Write-Host "Container $CONTAINER_NAME exists but is not running." -ForegroundColor Yellow
    }
} catch {
    Write-Host "Container $CONTAINER_NAME does not exist." -ForegroundColor Yellow
}

# Step 2: Remove container if it exists
Write-Host "Removing container if it exists..." -ForegroundColor Yellow
docker rm $CONTAINER_NAME 2>$null
if ($LASTEXITCODE -eq 0) {
    Write-Host "Container $CONTAINER_NAME removed successfully." -ForegroundColor Green
} else {
    Write-Host "No container named $CONTAINER_NAME to remove." -ForegroundColor Yellow
}

# Step 3: Ask if user wants to remove the Docker image as well
$removeImage = Read-Host "Do you want to remove the Docker image '$IMAGE_NAME' as well? (y/n)"
if ($removeImage -eq "y" -or $removeImage -eq "Y") {
    Write-Host "Removing Docker image $IMAGE_NAME..." -ForegroundColor Yellow
    docker rmi $IMAGE_NAME 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Docker image $IMAGE_NAME removed successfully." -ForegroundColor Green
    } else {
        Write-Host "Failed to remove Docker image $IMAGE_NAME. It may be in use or doesn't exist." -ForegroundColor Red
    }
} else {
    Write-Host "Keeping Docker image $IMAGE_NAME." -ForegroundColor Yellow
}

Write-Host "`n========== CLEANUP COMPLETED ==========`n" -ForegroundColor Green
Write-Host "Local Docker environment for GMH Leasing Dashboard has been cleaned up." -ForegroundColor Green
