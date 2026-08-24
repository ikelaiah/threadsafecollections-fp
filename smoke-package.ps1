[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$ExpectedVersion
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$RepositoryRoot = [System.IO.Path]::GetFullPath($PSScriptRoot)
$PackageDir = Join-Path $RepositoryRoot 'package/lazarus'
$PackageFile = Join-Path $PackageDir 'ThreadSafeCollections.lpk'
$SourceRoot = Join-Path $RepositoryRoot 'src'
$ConsumerSource = Join-Path $RepositoryRoot 'tools/package-smoke-consumer.lpr'
$SmokeDir = Join-Path $RepositoryRoot 'build-temp/package-smoke'
$LibDir = Join-Path $PackageDir 'lib'

$Lazbuild = Get-Command lazbuild -ErrorAction SilentlyContinue
if ($null -eq $Lazbuild) {
    $Candidate = 'C:\lazarus\lazbuild.exe'
    if (Test-Path -LiteralPath $Candidate) {
        $Lazbuild = Get-Item -LiteralPath $Candidate
    }
    else {
        throw 'lazbuild was not found on PATH and no default install was found at C:\lazarus\lazbuild.exe.'
    }
}
# Get-Command returns a CommandInfo (path in .Source); the C:\lazarus fallback
# returns a FileInfo (path in .FullName). Normalize to a plain executable path
# so Set-StrictMode does not trip over the missing .Source member.
if ($Lazbuild -is [System.Management.Automation.CommandInfo]) {
    $LazbuildExecutable = $Lazbuild.Source
}
else {
    $LazbuildExecutable = $Lazbuild.FullName
}
$Compiler = Get-Command fpc -ErrorAction Stop

[xml]$Package = Get-Content -LiteralPath $PackageFile -Raw
$VersionNode = $Package.SelectSingleNode('/CONFIG/Package/Version')
$Major = $VersionNode.GetAttribute('Major')
if ($Major -eq '') { $Major = '0' }
$Version = "$Major.$($VersionNode.GetAttribute('Minor')).$($VersionNode.GetAttribute('Release'))"
if ($Version -ne $ExpectedVersion) {
    throw "Package version is '$Version', expected '$ExpectedVersion'."
}
Write-Host "Package version: $Version (matches $ExpectedVersion)"

$SourceUnits = @(
    Get-ChildItem -LiteralPath $SourceRoot -File -Filter '*.pas' |
        Sort-Object Name
)
$ListedFiles = @(
    $Package.CONFIG.Package.Files.Item |
        ForEach-Object { [System.IO.Path]::GetFileName($_.SelectSingleNode('Filename').GetAttribute('Value')) }
)
foreach ($Unit in $SourceUnits) {
    if ($ListedFiles -notcontains $Unit.Name) {
        throw "FAIL: $($Unit.Name) is not listed in the package files."
    }
}
Write-Host "All $($SourceUnits.Count) source units are listed in the package."

Write-Host "Building the Lazarus package with lazbuild ($LazbuildExecutable)..."
& $LazbuildExecutable --build-all $PackageFile
if ($LASTEXITCODE -ne 0) {
    throw 'lazbuild failed while building the Lazarus package.'
}

$OutputDirs = @(Get-ChildItem -LiteralPath $LibDir -Directory)
if ($OutputDirs.Count -eq 0) {
    throw "FAIL: lazbuild produced no output directory below '$LibDir'."
}
$PackageUnits = $OutputDirs[0].FullName
Write-Host "Package units written to: $PackageUnits"

foreach ($Unit in $SourceUnits) {
    $UnitBase = [System.IO.Path]::GetFileNameWithoutExtension($Unit.Name)
    $PpuPath = Join-Path $PackageUnits "$UnitBase.ppu"
    if (-not (Test-Path -LiteralPath $PpuPath)) {
        throw "FAIL: expected compiled unit '$PpuPath' was not produced."
    }
}
Write-Host "All $($SourceUnits.Count) package units were compiled."

New-Item -ItemType Directory -Path (Join-Path $SmokeDir 'units'), (Join-Path $SmokeDir 'bin') -Force | Out-Null
Write-Host 'Compiling and running the tiny package consumer...'
& $Compiler.Source -B -MObjFPC -Sh `
    "-Fu$PackageUnits" `
    "-FU$(Join-Path $SmokeDir 'units')" `
    "-FE$(Join-Path $SmokeDir 'bin')" `
    $ConsumerSource
if ($LASTEXITCODE -ne 0) {
    throw 'Free Pascal failed while compiling the package smoke consumer.'
}

& (Join-Path $SmokeDir 'bin/package-smoke-consumer.exe')
if ($LASTEXITCODE -ne 0) {
    throw 'The package smoke consumer exited with a non-zero code.'
}

Write-Host 'Package smoke build passed.'
