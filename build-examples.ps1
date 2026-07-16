[CmdletBinding()]
param(
    [ValidateSet('Debug', 'Release')]
    [string]$Configuration = 'Release'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$RepositoryRoot = [System.IO.Path]::GetFullPath($PSScriptRoot)
$ExamplesRoot = Join-Path $RepositoryRoot 'examples'
$SourceRoot = Join-Path $RepositoryRoot 'src'
$OutputRoot = Join-Path $RepositoryRoot 'example-bin'
$UnitsRoot = Join-Path $OutputRoot 'units'
$Compiler = Get-Command fpc -ErrorAction Stop
$IsWindowsPlatform = [System.Environment]::OSVersion.Platform -eq
    [System.PlatformID]::Win32NT

$Projects = @(
    Get-ChildItem -LiteralPath $ExamplesRoot -Directory |
        ForEach-Object {
            Get-ChildItem -LiteralPath $_.FullName -File -Filter '*.lpr'
        } |
        Sort-Object FullName
)

if ($Projects.Count -eq 0) {
    throw "No example projects were found below '$ExamplesRoot'."
}

New-Item -ItemType Directory -Path $OutputRoot -Force | Out-Null
New-Item -ItemType Directory -Path $UnitsRoot -Force | Out-Null

$CompilerOptions = @('-B', '-MObjFPC', '-Sh')
if ($Configuration -eq 'Release') {
    $CompilerOptions += @('-O3', '-XX')
}
else {
    $CompilerOptions += @('-O1', '-gl', '-gh', '-Cr', '-Co')
}

$OutputNames = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
)

Write-Host "Compiling $($Projects.Count) examples ($Configuration)..."

foreach ($Project in $Projects) {
    $OutputName = $Project.BaseName
    if (-not $OutputNames.Add($OutputName)) {
        throw "More than one example would produce '$OutputName'. Rename one of the projects."
    }

    $UnitOutput = Join-Path $UnitsRoot $OutputName
    New-Item -ItemType Directory -Path $UnitOutput -Force | Out-Null
    if ($IsWindowsPlatform) {
        $ExecutableName = "$OutputName.exe"
    }
    else {
        $ExecutableName = $OutputName
    }

    $Arguments = @(
        $CompilerOptions
        "-Fu$SourceRoot"
        "-Fu$($Project.Directory.FullName)"
        "-FU$UnitOutput"
        "-FE$OutputRoot"
        "-o$ExecutableName"
        $Project.FullName
    )

    Write-Host "  -> $OutputName"
    & $Compiler.Source @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "Free Pascal failed while compiling '$($Project.FullName)'."
    }
}

Write-Host "Compiled $($Projects.Count) examples into '$OutputRoot'."
