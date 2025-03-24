####################################
# GMH Leasing Dashboard Config Secret Update Script
# This script updates the configuration secret in GCP Secret Manager
# without rebuilding or redeploying the application
####################################

# Variables
$PROJECT_ID = "gmh-communities"
$SECRET_NAME = "gmh-leasing-config"

# Set error action preference
$ErrorActionPreference = "Stop"

try {
    ####################################
    # STEP 1: Verify Configuration File
    ####################################
    Write-Host "`n========== CHECKING CONFIGURATION ==========`n" -ForegroundColor Cyan

    $configFile = "./config.yml"
    if (-not (Test-Path $configFile)) {
        throw "Configuration file not found at: $configFile"
    }

    Write-Host "Found configuration file: $configFile" -ForegroundColor Green

    ####################################
    # STEP 2: Authenticate with GCP
    ####################################
    Write-Host "`n========== AUTHENTICATING WITH GCP ==========`n" -ForegroundColor Cyan

    # Suppress the warning messages from the Google Cloud SDK's PowerShell scripts
    $originalErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = "SilentlyContinue"

    Write-Host "Authenticating with GCP..." -ForegroundColor Yellow
    gcloud auth login

    Write-Host "Setting project to $PROJECT_ID..." -ForegroundColor Yellow
    gcloud config set project $PROJECT_ID

    # Restore error action preference
    $ErrorActionPreference = $originalErrorActionPreference

    ####################################
    # STEP 3: Update Secret
    ####################################
    Write-Host "`n========== UPDATING SECRET IN SECRET MANAGER ==========`n" -ForegroundColor Cyan

    # Check if secret exists
    $secretExists = $false
    try {
        $secretInfo = gcloud secrets describe $SECRET_NAME --format="value(name)" 2>$null
        if ($secretInfo) {
            $secretExists = $true
            Write-Host "Secret $SECRET_NAME found in Secret Manager." -ForegroundColor Green
        }
    } catch {
        Write-Host "Secret $SECRET_NAME not found in Secret Manager." -ForegroundColor Red
        throw "Secret $SECRET_NAME does not exist. Run deploy-prelease.ps1 first to create it."
    }

    if ($secretExists) {
        # Add new version with config.yml content
        Write-Host "Adding new version to secret: $SECRET_NAME..." -ForegroundColor Yellow
        gcloud secrets versions add $SECRET_NAME --data-file="$configFile"

        if ($LASTEXITCODE -ne 0) {
            throw "Failed to add new version to secret $SECRET_NAME"
        }

        Write-Host "Secret successfully updated with new version!" -ForegroundColor Green

        # Get the latest version number
        $latestVersion = gcloud secrets versions list $SECRET_NAME --limit=1 --format="value(name)" 2>$null
        Write-Host "Latest version is: $latestVersion" -ForegroundColor Green
    }

    ####################################
    # STEP 4: Completion
    ####################################
    Write-Host "`n========== CONFIG UPDATE COMPLETED SUCCESSFULLY ==========`n" -ForegroundColor Green
    Write-Host "Configuration has been updated in Secret Manager." -ForegroundColor Green
    Write-Host "The next time your Cloud Run service restarts, it will use the updated configuration." -ForegroundColor Green
    Write-Host "If you need the changes to take effect immediately, you can manually restart the service using:" -ForegroundColor Yellow
    Write-Host "gcloud run services update gmh-leasing-dashboard-prod --region=us-east1" -ForegroundColor Yellow

} catch {
    Write-Host "`n========== ERROR ==========`n" -ForegroundColor Red
    Write-Host "Error: $_" -ForegroundColor Red
    Write-Host "Stack Trace: $($_.ScriptStackTrace)" -ForegroundColor Red
    exit 1
}
