[CmdletBinding()]
param(
    [switch]$SkipLeakCheck
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$RepositoryRoot = [System.IO.Path]::GetFullPath($PSScriptRoot)
$Compiler = Get-Command fpc -ErrorAction Stop
$TestsRoot = Join-Path $RepositoryRoot 'tests'
$SourceRoot = Join-Path $RepositoryRoot 'src'
$UnitsOut = Join-Path $RepositoryRoot 'build-temp/tests/units'
$BinOut = Join-Path $RepositoryRoot 'build-temp/tests/bin'

New-Item -ItemType Directory -Path $UnitsOut, $BinOut -Force | Out-Null

Write-Host "Compiling the FPCUnit test runner (debug checks + HeapTrc)..."
& $Compiler.Source -B -MObjFPC -Sh -gl -gh -Cr -Co `
    "-Fu$SourceRoot" `
    "-Fu$TestsRoot" `
    "-FU$UnitsOut" `
    "-FE$BinOut" `
    (Join-Path $TestsRoot 'TestRunner.lpr')
if ($LASTEXITCODE -ne 0) {
    throw "Free Pascal failed while compiling tests/TestRunner.lpr."
}

$Runner = Join-Path $BinOut 'TestRunner.exe'
$OutputFile = Join-Path $BinOut 'test-output.txt'

Write-Host "Running the FPCUnit suite (collision and stress cases can take several minutes)..."
& $Runner --all --format=plain *> $OutputFile
$RunnerExitCode = $LASTEXITCODE

$Text = Get-Content -LiteralPath $OutputFile -Raw

function Get-MatchValue {
    param([string]$Text, [string]$Pattern)
    $Match = [regex]::Match($Text, $Pattern)
    if ($Match.Success) {
        return [int]$Match.Groups[1].Value
    }
    return $null
}

$RunTests = Get-MatchValue $Text 'Number of run tests:\s+(\d+)'
$Errors = Get-MatchValue $Text 'Number of errors:\s+(\d+)'
$Failures = Get-MatchValue $Text 'Number of failures:\s+(\d+)'
$Unfreed = Get-MatchValue $Text '(\d+) unfreed memory blocks'

Write-Host ''
Write-Host 'FPCUnit summary:'
if ($null -eq $RunTests) { Write-Host '  run tests : <not found>' } else { Write-Host "  run tests : $RunTests" }
if ($null -eq $Errors) { Write-Host '  errors    : <not found>' } else { Write-Host "  errors    : $Errors" }
if ($null -eq $Failures) { Write-Host '  failures  : <not found>' } else { Write-Host "  failures  : $Failures" }
if ($null -eq $Unfreed) { Write-Host '  HeapTrc   : <not found>' } else { Write-Host "  HeapTrc   : $Unfreed unfreed memory blocks" }

$Failed = $false

if ($null -eq $RunTests -or $null -eq $Errors -or $null -eq $Failures) {
    Write-Host ''
    Write-Host "FAIL: could not parse the FPCUnit summary from '$OutputFile'."
    Write-Host 'Full output:'
    Get-Content -LiteralPath $OutputFile
    exit 1
}

if ($RunnerExitCode -ne 0) {
    Write-Host "The test runner exited with code $RunnerExitCode."
    $Failed = $true
}

if ($Errors -gt 0 -or $Failures -gt 0) {
    Write-Host 'The FPCUnit suite reported errors or failures.'
    $Failed = $true
}

if (-not $SkipLeakCheck) {
    if ($null -eq $Unfreed) {
        Write-Host 'FAIL: no HeapTrc summary was found. The runner must be built with -gh'
        Write-Host 'and exit normally for leak verification.'
        $Failed = $true
    }
    elseif ($Unfreed -gt 0) {
        Write-Host "FAIL: HeapTrc reports $Unfreed unfreed memory blocks."
        $Failed = $true
    }
}

if ($Failed) {
    Write-Host ''
    Write-Host "FAILURE: the FPCUnit suite reported problems. Full output: $OutputFile"
    exit 1
}

Write-Host ''
Write-Host 'SUCCESS: the FPCUnit suite passed, and HeapTrc reported no unfreed blocks.'
