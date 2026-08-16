[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$ExpectedVersion
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$RepoRoot = [System.IO.Path]::GetFullPath(
    (Join-Path (Split-Path -Parent $MyInvocation.MyCommand.Path) '..'))
$DocsRoot = Join-Path $RepoRoot 'docs'
$Issues = [System.Collections.Generic.List[string]]::new()

function Add-Issue {
    param([string]$Message)
    $Issues.Add($Message)
}

function Test-Match {
    param(
        [string]$File,
        [string]$What,
        [string]$Pattern
    )
    $Text = Get-Content -LiteralPath $File -Raw
    $IsMatch = [regex]::IsMatch(
        $Text, $Pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
    if (-not $IsMatch) {
        Add-Issue "$File : $What"
    }
}

$Readme = Join-Path $RepoRoot 'README.md'
$ReadmeText = Get-Content -LiteralPath $Readme -Raw
$BadgeMatch = [regex]::Match($ReadmeText, 'version-(\d+\.\d+\.\d+)')
if (-not $BadgeMatch.Success -or $BadgeMatch.Groups[1].Value -ne $ExpectedVersion) {
    Add-Issue 'README.md : version badge does not match the expected version'
}

$PackageFile = Join-Path $RepoRoot 'package/lazarus/ThreadSafeCollections.lpk'
[xml]$Package = Get-Content -LiteralPath $PackageFile -Raw
$VersionNode = $Package.SelectSingleNode('/CONFIG/Package/Version')
$Major = $VersionNode.GetAttribute('Major')
if ($Major -eq '') { $Major = '0' }
$PackageVersion = "$Major.$($VersionNode.GetAttribute('Minor')).$($VersionNode.GetAttribute('Release'))"
if ($PackageVersion -ne $ExpectedVersion) {
    Add-Issue "$PackageFile : package version '$PackageVersion' does not match '$ExpectedVersion'"
}

$DocsHome = Join-Path $DocsRoot 'README.md'
Test-Match $DocsHome "documentation home does not mention version $ExpectedVersion" `
    ('ThreadSafeCollections-FP ' + [regex]::Escape($ExpectedVersion))
Test-Match $DocsHome "documentation home does not link RELEASE-NOTES-v$ExpectedVersion.md" `
    ('RELEASE-NOTES-v' + [regex]::Escape($ExpectedVersion) + '\.md')
Test-Match $DocsHome "documentation home does not link PR_v$ExpectedVersion.md" `
    ('PR_v' + [regex]::Escape($ExpectedVersion) + '\.md')

$Changelog = Join-Path $RepoRoot 'CHANGELOG.md'
Test-Match $Changelog "CHANGELOG.md has no '$ExpectedVersion' entry" `
    ('^\s*## \[' + [regex]::Escape($ExpectedVersion) + '\]')

$ReleaseNotes = Join-Path $DocsRoot "RELEASE-NOTES-v$ExpectedVersion.md"
if (-not (Test-Path -LiteralPath $ReleaseNotes)) {
    Add-Issue "missing $ReleaseNotes"
}
else {
    Test-Match $ReleaseNotes 'release notes title does not mention the expected version' `
        ('v' + [regex]::Escape($ExpectedVersion))
}

$CheatSheet = Join-Path $DocsRoot 'CHEATSHEET.md'
Test-Match $CheatSheet 'generated cheat sheet does not report the expected package version' `
    ('Package version: `' + [regex]::Escape($ExpectedVersion) + '`')

if ($Issues.Count -gt 0) {
    Write-Host "Release metadata checks for v$ExpectedVersion found problems:"
    foreach ($Issue in $Issues) {
        Write-Host "  - $Issue"
    }
    exit 1
}

Write-Host "Release metadata checks for v$ExpectedVersion passed."
