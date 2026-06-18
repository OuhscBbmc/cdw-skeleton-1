# run-python.ps1
# Finds or installs Python, then runs the script passed as arguments.
# Usage: utility\run-python.ps1 utility\export-repo-issues.py [args...]

param(
    [Parameter(Mandatory, Position=0, ValueFromRemainingArguments)]
    [string[]]$ScriptAndArgs
)

function Find-Python {
    $candidates = @(
        "python",
        "py",
        "$env:LOCALAPPDATA\Microsoft\WindowsApps\python.exe",
        "$env:LOCALAPPDATA\Microsoft\WindowsApps\python3.exe",
        "$env:LOCALAPPDATA\Programs\Python\Python311\python.exe",
        "$env:LOCALAPPDATA\Programs\Python\Python312\python.exe",
        "$env:LOCALAPPDATA\Programs\Python\Python310\python.exe"
    )
    foreach ($cmd in $candidates) {
        try {
            $ver = & $cmd --version 2>&1
            if ($LASTEXITCODE -eq 0 -and $ver -match "Python 3") {
                return $cmd
            }
        } catch {}
    }
    return $null
}

function Install-Python {
    Write-Host "Python not found. Attempting to install via winget..." -ForegroundColor Yellow
    if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
        Write-Host "winget not available. Please install Python 3 from https://www.python.org/downloads/ and re-run." -ForegroundColor Red
        exit 1
    }
    winget install --id Python.Python.3.11 --exact --silent --accept-package-agreements --accept-source-agreements
    if ($LASTEXITCODE -ne 0) {
        Write-Host "winget install failed. Please install Python 3 manually from https://www.python.org/downloads/" -ForegroundColor Red
        exit 1
    }
    # Refresh PATH so newly installed python is findable
    $env:PATH = [System.Environment]::GetEnvironmentVariable("PATH", "Machine") + ";" +
                [System.Environment]::GetEnvironmentVariable("PATH", "User")
    $py = Find-Python
    if (-not $py) {
        Write-Host "Python installed but still not on PATH. Close and reopen this terminal, then re-run." -ForegroundColor Yellow
        exit 1
    }
    return $py
}

$python = Find-Python
if (-not $python) {
    $python = Install-Python
}

& $python @ScriptAndArgs
exit $LASTEXITCODE
