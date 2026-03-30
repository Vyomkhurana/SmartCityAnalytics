param(
  [switch]$SkipSetup,
  [switch]$SkipPipeline,
  [switch]$LaunchDashboard
)

$ErrorActionPreference = "Stop"

function Find-Rscript {
  $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }

  $candidatePaths = @(
    "C:\Program Files\R",
    "C:\Program Files (x86)\R"
  )

  foreach ($base in $candidatePaths) {
    if (Test-Path $base) {
      $bins = Get-ChildItem -Path $base -Directory -ErrorAction SilentlyContinue |
        Sort-Object Name -Descending |
        ForEach-Object {
          @(
            (Join-Path $_.FullName "bin\Rscript.exe"),
            (Join-Path $_.FullName "bin\x64\Rscript.exe")
          )
        }

      foreach ($bin in $bins) {
        if (Test-Path $bin) {
          return $bin
        }
      }
    }
  }

  return $null
}

$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $projectRoot

$rscript = Find-Rscript
if (-not $rscript) {
  Write-Host "Rscript not found." -ForegroundColor Red
  Write-Host "Install R and ensure Rscript.exe is on PATH, or install R in Program Files." -ForegroundColor Yellow
  exit 1
}

Write-Host "Using Rscript: $rscript" -ForegroundColor Cyan
& $rscript --version

if (-not $SkipSetup) {
  Write-Host "\n[1/3] Installing/checking required R packages..." -ForegroundColor Green
  & $rscript "setup.R"
}
else {
  Write-Host "\n[1/3] Skipped package setup (-SkipSetup)." -ForegroundColor DarkYellow
}

if (-not $SkipPipeline) {
  Write-Host "\n[2/3] Running full analytics pipeline..." -ForegroundColor Green
  & $rscript "run_pipeline.R"
}
else {
  Write-Host "\n[2/3] Skipped pipeline run (-SkipPipeline)." -ForegroundColor DarkYellow
}

if ($LaunchDashboard) {
  Write-Host "\n[3/3] Launching Shiny dashboard..." -ForegroundColor Green
  & $rscript -e "shiny::runApp('shiny_app')"
}
else {
  Write-Host "\n[3/3] Dashboard not launched. Use -LaunchDashboard to start it." -ForegroundColor DarkYellow
}

Write-Host "\nDone." -ForegroundColor Cyan
