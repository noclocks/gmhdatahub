####################################
# GMH Leasing Dashboard Local Docker Testing Script
# This script builds and runs a Docker container locally with
# the configuration file mounted as a volume
####################################

# Variables
$IMAGE_NAME = "gmhdatahub-local"
$CONTAINER_NAME = "gmhdatahub-test"
$CONFIG_PATH = (Get-Location).Path
$PORT = 8080

# Set error action preference
$ErrorActionPreference = "Stop"

try {
    ####################################
    # STEP 1: Verify Configuration File
    ####################################
    Write-Host "`n========== CHECKING CONFIGURATION ==========`n" -ForegroundColor Cyan

    $configFile = "${CONFIG_PATH}/config.yml"
    if (-not (Test-Path $configFile)) {
        throw "Configuration file not found at: $configFile"
    }

    Write-Host "Found configuration file: $configFile" -ForegroundColor Green

    ####################################
    # STEP 2: Build Docker Image
    ####################################
    Write-Host "`n========== BUILDING DOCKER IMAGE ==========`n" -ForegroundColor Cyan

    Write-Host "Building Docker image: $IMAGE_NAME..." -ForegroundColor Yellow
    docker build --file .\Dockerfile.prelease -t $IMAGE_NAME .

    if ($LASTEXITCODE -ne 0) {
        throw "Docker build failed"
    }

    Write-Host "Docker image built successfully" -ForegroundColor Green

    ####################################
    # STEP 3: Clean Up Existing Containers
    ####################################
    Write-Host "`n========== CLEANING UP EXISTING CONTAINERS ==========`n" -ForegroundColor Cyan

    # Check if container exists and is running
    $containerRunning = $false
    try {
        $containerStatus = docker inspect --format="{{.State.Running}}" $CONTAINER_NAME 2>$null
        $containerRunning = ($containerStatus -eq "true")
    } catch {
        # Container doesn't exist, which is fine
    }

    if ($containerRunning) {
        Write-Host "Stopping existing container: $CONTAINER_NAME..." -ForegroundColor Yellow
        docker stop $CONTAINER_NAME >$null
    }

    # Try to remove the container if it exists
    try {
        $containerExists = docker inspect $CONTAINER_NAME >$null 2>&1
        if ($?) {
            Write-Host "Removing existing container: $CONTAINER_NAME..." -ForegroundColor Yellow
            docker rm $CONTAINER_NAME >$null
        }
    } catch {
        # Container doesn't exist, which is fine
    }

    ####################################
    # STEP 4: Run Docker Container
    ####################################
    Write-Host "`n========== STARTING DOCKER CONTAINER ==========`n" -ForegroundColor Cyan

    Write-Host "Starting container with mounted configuration..." -ForegroundColor Yellow

    # Run Docker container with config.yml mounted as a volume
    docker run -d `
        --name $CONTAINER_NAME `
        -p ${PORT}:${PORT} `
        -e "SHINY_PORT=${PORT}" `
        -e "SHINY_HOST=0.0.0.0" `
        -e "R_CONFIG_ACTIVE=default" `
        -v "${CONFIG_PATH}/config.yml:/secrets/config.yml:ro" `
        $IMAGE_NAME

    if ($LASTEXITCODE -ne 0) {
        throw "Failed to start Docker container"
    }

    # Wait a moment to make sure container is running
    Start-Sleep -Seconds 2

    # Verify container is running
    $containerStatus = docker inspect --format="{{.State.Running}}" $CONTAINER_NAME
    if ($containerStatus -ne "true") {
        throw "Container is not running. Check logs with: docker logs $CONTAINER_NAME"
    }

    ####################################
    # STEP 5: Show Access Information
    ####################################
    Write-Host "`n========== CONTAINER RUNNING SUCCESSFULLY ==========`n" -ForegroundColor Green

    Write-Host "GMH Leasing Dashboard is now running in a Docker container" -ForegroundColor Green
    Write-Host "Access the application at: http://localhost:${PORT}" -ForegroundColor Cyan
    Write-Host "`nUseful commands:" -ForegroundColor Yellow
    Write-Host "- View logs:          docker logs $CONTAINER_NAME" -ForegroundColor Yellow
    Write-Host "- Access shell:       docker exec -it $CONTAINER_NAME bash" -ForegroundColor Yellow
    Write-Host "- Stop container:     docker stop $CONTAINER_NAME" -ForegroundColor Yellow
    Write-Host "- Verify config:      docker exec $CONTAINER_NAME ls -la /secrets/config.yml" -ForegroundColor Yellow

} catch {
    Write-Host "`n========== ERROR ==========`n" -ForegroundColor Red
    Write-Host "Error: $_" -ForegroundColor Red
    Write-Host "Stack Trace: $($_.ScriptStackTrace)" -ForegroundColor Red
    exit 1
}
