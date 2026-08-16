[CmdletBinding()]
param()

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

function ConvertTo-GitHubAnchor {
    param([string]$Heading)
    $Lower = $Heading.ToLowerInvariant()
    $Builder = [System.Text.StringBuilder]::new()
    foreach ($Char in $Lower.ToCharArray()) {
        if ([char]::IsLetterOrDigit($Char)) {
            [void]$Builder.Append($Char)
        }
        elseif ($Char -eq ' ') {
            [void]$Builder.Append('-')
        }
        elseif ($Char -eq '-' -or $Char -eq '_') {
            [void]$Builder.Append($Char)
        }
    }
    $Builder.ToString()
}

function Get-Headings {
    param([string]$Path)
    $Anchors = [System.Collections.Generic.HashSet[string]]::new(
        [System.StringComparer]::Ordinal)
    foreach ($Line in Get-Content -LiteralPath $Path) {
        $Match = [regex]::Match($Line, '^\s{0,3}#{1,6}\s+(.+?)\s*#*\s*$')
        if ($Match.Success) {
            [void]$Anchors.Add((ConvertTo-GitHubAnchor $Match.Groups[1].Value))
        }
    }
    $Anchors
}

function Test-LinkTarget {
    param(
        [string]$MarkdownFile,
        [string]$SourceLine,
        [string]$Destination
    )

    $Fragment = ''
    $HashIndex = $Destination.IndexOf('#')
    if ($HashIndex -ge 0) {
        $Fragment = $Destination.Substring($HashIndex + 1)
        $Destination = $Destination.Substring(0, $HashIndex)
    }

    if ($Destination -eq '') {
        $TargetPath = $MarkdownFile
    }
    else {
        $TargetPath = [System.IO.Path]::GetFullPath(
            (Join-Path (Split-Path -Parent $MarkdownFile) $Destination))
        if (-not (Test-Path -LiteralPath $TargetPath)) {
            Add-Issue "$MarkdownFile : link target does not exist: '$Destination' ($SourceLine)"
            return
        }
    }

    if ($Fragment -ne '') {
        $Fragment = [Uri]::UnescapeDataString($Fragment)
        if ([System.IO.Path]::GetExtension($TargetPath) -eq '.md') {
            $Anchors = @(Get-Headings $TargetPath)
            if ($Anchors -notcontains $Fragment) {
                Add-Issue "$MarkdownFile : broken heading fragment '#$Fragment' in '$Destination' ($SourceLine)"
            }
        }
        elseif ($Fragment -notmatch '^L\d+(-L\d+)?$') {
            Add-Issue "$MarkdownFile : unrecognized fragment '#$Fragment' in '$Destination' ($SourceLine)"
        }
    }
}

$MarkdownFiles = @(
    @(Get-ChildItem -LiteralPath $RepoRoot -File -Filter '*.md') +
    @(Get-ChildItem -LiteralPath $DocsRoot -File -Filter '*.md')
)

foreach ($File in $MarkdownFiles) {
    $Lines = Get-Content -LiteralPath $File.FullName
    $FenceCount = 0
    $LineNumber = 0
    foreach ($Line in $Lines) {
        $LineNumber++
        if ($Line -match '^\s*`{3,}') {
            $FenceCount++
        }
        $InlineMatches = [regex]::Matches($Line, '\[[^\]]*\]\(([^)\s]+)(?:\s+"[^"]*")?\)')
        foreach ($Match in $InlineMatches) {
            $Destination = $Match.Groups[1].Value.TrimStart('<').TrimEnd('>')
            if ($Destination -match '^(https?://|mailto:|//|ftp://)') {
                continue
            }
            Test-LinkTarget $File.FullName "line $LineNumber" $Destination
        }
        $ReferenceMatches = [regex]::Matches($Line, '^\s*\[[^\]]+\]:\s*(\S+)')
        foreach ($Match in $ReferenceMatches) {
            $Destination = $Match.Groups[1].Value.TrimStart('<').TrimEnd('>')
            if ($Destination -match '^(https?://|mailto:|//|ftp://)') {
                continue
            }
            Test-LinkTarget $File.FullName "line $LineNumber" $Destination
        }
    }
    if ($FenceCount % 2 -ne 0) {
        Add-Issue "$($File.FullName) : unbalanced code fences ($FenceCount fence markers found)"
    }
}

$ExampleLprFiles = @(
    Get-ChildItem -LiteralPath (Join-Path $RepoRoot 'examples') -Recurse -File -Filter '*.lpr' |
        Where-Object { $_.FullName -split '[\\/]' -notcontains 'backup' }
)
$BuildingText = Get-Content -LiteralPath (Join-Path $DocsRoot 'BUILDING.md') -Raw
foreach ($Example in $ExampleLprFiles) {
    if ($BuildingText -notmatch [regex]::Escape($Example.BaseName)) {
        Add-Issue "examples/$($Example.BaseName) is not documented in docs/BUILDING.md"
    }
}
$BuildingLines = $BuildingText -split "`n"
$LineNumber = 0
foreach ($Line in $BuildingLines) {
    $LineNumber++
    if ($Line -match '^\|\s*`([^`]+)`') {
        $Name = $Matches[1]
        $Found = $ExampleLprFiles | Where-Object { $_.BaseName -eq $Name }
        if ($null -eq $Found) {
            Add-Issue "docs/BUILDING.md line $LineNumber : no example named '$Name' exists"
        }
    }
}

$CheckSheetPath = Join-Path $RepoRoot 'build-temp/cheatsheet-check.md'
$Generator = Join-Path $RepoRoot 'tools/generate-cheatsheet.ps1'
& pwsh -NoProfile -File $Generator -OutputPath $CheckSheetPath
if ($LASTEXITCODE -ne 0) {
    Add-Issue 'tools/generate-cheatsheet.ps1 failed to run'
}
elseif (Test-Path -LiteralPath $CheckSheetPath) {
    $Current = Get-Content -LiteralPath (Join-Path $DocsRoot 'CHEATSHEET.md') -Raw
    $Generated = Get-Content -LiteralPath $CheckSheetPath -Raw
    if ($Current -ne $Generated) {
        Add-Issue 'docs/CHEATSHEET.md is stale; regenerate with pwsh -File ./tools/generate-cheatsheet.ps1'
    }
}
else {
    Add-Issue 'cheat-sheet check produced no output file'
}

if ($Issues.Count -gt 0) {
    Write-Host 'Documentation checks found problems:'
    foreach ($Issue in $Issues) {
        Write-Host "  - $Issue"
    }
    exit 1
}

Write-Host 'Documentation checks passed:'
Write-Host "  - $($MarkdownFiles.Count) Markdown files checked for links and fences"
Write-Host "  - $($ExampleLprFiles.Count) examples cross-checked against docs/BUILDING.md"
Write-Host '  - generated cheat sheet matches docs/CHEATSHEET.md'
