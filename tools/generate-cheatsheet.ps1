param(
  [string]$OutputPath
)

$ErrorActionPreference = 'Stop'

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent $ScriptDir

if ([string]::IsNullOrWhiteSpace($OutputPath)) {
  $OutputPath = Join-Path $RepoRoot 'docs/CHEATSHEET.md'
}
elseif (-not [System.IO.Path]::IsPathRooted($OutputPath)) {
  $OutputPath = Join-Path $RepoRoot $OutputPath
}

function Read-RepoFile {
  param([string]$RelativePath)
  Get-Content -LiteralPath (Join-Path $RepoRoot $RelativePath) -Raw
}

function ConvertTo-RepoPath {
  param([string]$Path)
  $fullPath = [System.IO.Path]::GetFullPath($Path)
  $relative = $fullPath.Substring($RepoRoot.Length).TrimStart('\', '/')
  $relative -replace '\\', '/'
}

function Get-PackageVersion {
  $packagePath = Join-Path $RepoRoot 'package/lazarus/ThreadSafeCollections.lpk'
  [xml]$xml = Get-Content -LiteralPath $packagePath -Raw
  $versionNode = $xml.CONFIG.Package.Version | Where-Object { $_.Minor -or $_.Release } | Select-Object -First 1
  if ($null -eq $versionNode) {
    return 'unknown'
  }

  $major = if ($versionNode.Major) { [string]$versionNode.Major } else { '0' }
  $minor = if ($versionNode.Minor) { [string]$versionNode.Minor } else { '0' }
  $release = if ($versionNode.Release) { [string]$versionNode.Release } else { '0' }
  "$major.$minor.$release"
}

function Get-InterfaceBlock {
  param(
    [string]$Text,
    [string]$InterfaceName,
    [switch]$Generic
  )

  if ($Generic) {
    $pattern = "(?ms)generic\s+$InterfaceName\s*<[^>]+>\s*=\s*interface(?:\([^)]*\))?.*?^\s*end;"
  }
  else {
    $pattern = "(?ms)$InterfaceName\s*=\s*interface(?:\([^)]*\))?.*?^\s*end;"
  }

  $match = [regex]::Match($Text, $pattern)
  if (-not $match.Success) {
    throw "Could not find interface block for $InterfaceName"
  }
  $match.Value
}

function Get-InterfaceMembers {
  param(
    [string]$Text,
    [string]$InterfaceName,
    [switch]$Generic
  )

  $block = Get-InterfaceBlock -Text $Text -InterfaceName $InterfaceName -Generic:$Generic
  $complexity = ''
  $members = New-Object System.Collections.Generic.List[object]

  foreach ($line in ($block -split "`r?`n")) {
    $trimmed = $line.Trim()
    if ($trimmed -match '^///\s*<complexity>(.*?)</complexity>') {
      $complexity = $matches[1]
      continue
    }

    if ($trimmed -cmatch '^(function|procedure|property)\s+.+;') {
      $members.Add([pscustomobject]@{
        Declaration = $trimmed
        Complexity = $complexity
      })
      $complexity = ''
    }
  }

  $accessorComplexity = @{}
  foreach ($member in $members) {
    if (-not $member.Complexity) {
      continue
    }

    $declaration = [string]$member.Declaration
    if ($declaration -cmatch '^(function|procedure)\s+([A-Za-z_][A-Za-z0-9_]*)\b') {
      $accessorComplexity[$matches[2]] = $member.Complexity
    }
  }

  foreach ($member in $members) {
    if ($member.Complexity) {
      continue
    }

    $declaration = [string]$member.Declaration
    if ($declaration -cnotmatch '^property\s+') {
      continue
    }

    $readComplexity = $null
    $writeComplexity = $null
    if ($declaration -cmatch '\bread\s+([A-Za-z_][A-Za-z0-9_]*)') {
      $readName = $matches[1]
      if ($accessorComplexity.ContainsKey($readName)) {
        $readComplexity = $accessorComplexity[$readName]
      }
    }
    if ($declaration -cmatch '\bwrite\s+([A-Za-z_][A-Za-z0-9_]*)') {
      $writeName = $matches[1]
      if ($accessorComplexity.ContainsKey($writeName)) {
        $writeComplexity = $accessorComplexity[$writeName]
      }
    }

    if ($readComplexity -and $writeComplexity) {
      if ($readComplexity -eq $writeComplexity) {
        $member.Complexity = $readComplexity
      }
      else {
        $member.Complexity = "read $readComplexity, write $writeComplexity"
      }
    }
    elseif ($readComplexity) {
      $member.Complexity = $readComplexity
    }
    elseif ($writeComplexity) {
      $member.Complexity = $writeComplexity
    }
  }

  $members
}

function ConvertTo-ApiStatements {
  param([string]$Text)

  $interfaceText = ($Text -split '(?m)^\s*implementation\s*$')[0]
  $statements = New-Object System.Collections.Generic.List[string]
  $current = ''
  $parenDepth = 0

  function Get-ParenDelta {
    param([string]$Value)
    $opens = ([regex]::Matches($Value, '\(')).Count
    $closes = ([regex]::Matches($Value, '\)')).Count
    $opens - $closes
  }

  foreach ($line in ($interfaceText -split "`r?`n")) {
    $trimmed = ($line -replace '\s*//.*$', '').Trim()
    if ([string]::IsNullOrWhiteSpace($trimmed)) {
      continue
    }

    if ($current) {
      $current = "$current $trimmed"
      $parenDepth += Get-ParenDelta $trimmed
      if ($trimmed.EndsWith(';') -and $parenDepth -le 0) {
        $statements.Add(($current -replace '\s+', ' ').Trim())
        $current = ''
        $parenDepth = 0
      }
      continue
    }

    if ($trimmed -cmatch '^(constructor|destructor|function|procedure|property)\s+') {
      $current = $trimmed
      $parenDepth = Get-ParenDelta $trimmed
      if ($trimmed.EndsWith(';') -and $parenDepth -le 0) {
        $statements.Add(($current -replace '\s+', ' ').Trim())
        $current = ''
        $parenDepth = 0
      }
    }
  }

  $statements
}

function Select-Constructors {
  param([string[]]$Statements)
  $Statements |
    Where-Object { $_ -cmatch '^constructor\s+Create\b' } |
    Where-Object { $_ -notmatch '\bA(List|Set|Deque|Dictionary)\b' } |
    Select-Object -Unique
}

function Select-DeclarationsByName {
  param(
    [string[]]$Statements,
    [string[]]$Names
  )

  $namePattern = ($Names | ForEach-Object { [regex]::Escape($_) }) -join '|'
  $Statements |
    Where-Object { $_ -cmatch "^(function|procedure|property)\s+($namePattern)\b" } |
    Select-Object -Unique
}

function Select-GlobalFunctions {
  param(
    [string[]]$Statements,
    [string[]]$Names
  )

  Select-DeclarationsByName -Statements $Statements -Names $Names
}

function Add-MemberTable {
  param(
    [System.Collections.Generic.List[string]]$Lines,
    [string]$Title,
    [object[]]$Members
  )

  $Lines.Add("### $Title")
  $Lines.Add('')
  $Lines.Add('| Complexity | Declaration |')
  $Lines.Add('|---|---|')
  foreach ($member in $Members) {
    $complexity = if ($member.Complexity) { $member.Complexity } else { '' }
    $decl = ([string]$member.Declaration).Replace('|', '\|')
    $Lines.Add('| ' + $complexity + ' | `' + $decl + '` |')
  }
  $Lines.Add('')
}

function Add-DeclarationList {
  param(
    [System.Collections.Generic.List[string]]$Lines,
    [string]$Title,
    [string[]]$Declarations
  )

  $Lines.Add("### $Title")
  $Lines.Add('')
  if ($Declarations.Count -eq 0) {
    $Lines.Add('- None found.')
  }
  else {
    foreach ($declaration in $Declarations) {
      $Lines.Add('- `' + $declaration + '`')
    }
  }
  $Lines.Add('')
}

$interfacesText = Read-RepoFile 'src/ThreadSafeCollections.Interfaces.pas'
$listText = Read-RepoFile 'src/ThreadSafeCollections.List.pas'
$dequeText = Read-RepoFile 'src/ThreadSafeCollections.Deque.pas'
$dictionaryText = Read-RepoFile 'src/ThreadSafeCollections.Dictionary.pas'
$hashSetText = Read-RepoFile 'src/ThreadSafeCollections.HashSet.pas'
$hashFunctionsText = Read-RepoFile 'src/HashFunctions.pas'

$listStatements = ConvertTo-ApiStatements $listText
$dequeStatements = ConvertTo-ApiStatements $dequeText
$dictionaryStatements = ConvertTo-ApiStatements $dictionaryText
$hashSetStatements = ConvertTo-ApiStatements $hashSetText
$hashFunctionStatements = ConvertTo-ApiStatements $hashFunctionsText

$sourceFiles = @(
  'src/ThreadSafeCollections.Interfaces.pas',
  'src/ThreadSafeCollections.List.pas',
  'src/ThreadSafeCollections.Deque.pas',
  'src/ThreadSafeCollections.Dictionary.pas',
  'src/ThreadSafeCollections.HashSet.pas',
  'src/HashFunctions.pas',
  'package/lazarus/ThreadSafeCollections.lpk'
)

$lines = New-Object System.Collections.Generic.List[string]
$lines.Add('# ThreadSafeCollections-FP Cheat Sheet')
$lines.Add('')
$lines.Add('[Documentation home](README.md) · [Project README](../README.md) · [Build and verify](BUILDING.md)')
$lines.Add('')
$lines.Add('**Audience:** developers who need a generated, compact reference to the current public API and source complexity annotations.')
$lines.Add('')
$lines.Add('> Generated file. Do not edit manually.')
$lines.Add('>')
$lines.Add('> Regenerate from the repository root with: `pwsh -File ./tools/generate-cheatsheet.ps1`')
$lines.Add('')
$lines.Add('Package version: `' + (Get-PackageVersion) + '`')
$lines.Add('')
$lines.Add('## Source Inputs')
$lines.Add('')
foreach ($file in $sourceFiles) {
  $lines.Add('- `' + $file + '`')
}
$lines.Add('')
$lines.Add('## Collection Types')
$lines.Add('')
$lines.Add('| Collection | Unit | Primary storage | Iteration model |')
$lines.Add('|---|---|---|---|')
$lines.Add('| `TThreadSafeList<T>` | `ThreadSafeCollections.List` | Dynamic array | Holds lock for full `for..in` loop |')
$lines.Add('| `TThreadSafeDeque<T>` | `ThreadSafeCollections.Deque` | Circular array | Holds lock for full `for..in` loop |')
$lines.Add('| `TThreadSafeDictionary<TKey, TValue>` | `ThreadSafeCollections.Dictionary` | Bucket array with chained entries | Snapshot at enumerator construction |')
$lines.Add('| `TThreadSafeHashSet<T>` | `ThreadSafeCollections.HashSet` | Bucket array with chained entries | Holds lock for full `for..in` loop |')
$lines.Add('')
$lines.Add('## Shared Interfaces')
$lines.Add('')
Add-MemberTable $lines 'IThreadSafeCollection<T>' (Get-InterfaceMembers -Text $interfacesText -InterfaceName 'IThreadSafeCollection' -Generic)
Add-MemberTable $lines 'IThreadSafeList<T>' (Get-InterfaceMembers -Text $interfacesText -InterfaceName 'IThreadSafeList' -Generic)
Add-MemberTable $lines 'IThreadSafeDeque<T>' (Get-InterfaceMembers -Text $interfacesText -InterfaceName 'IThreadSafeDeque' -Generic)
Add-MemberTable $lines 'IThreadSafeDictionary<TKey, TValue>' (Get-InterfaceMembers -Text $interfacesText -InterfaceName 'IThreadSafeDictionary' -Generic)
Add-MemberTable $lines 'IThreadSafeHashSet<T>' (Get-InterfaceMembers -Text $interfacesText -InterfaceName 'IThreadSafeHashSet' -Generic)

$lines.Add('## Constructors')
$lines.Add('')
Add-DeclarationList $lines 'List constructors' (Select-Constructors $listStatements)
Add-DeclarationList $lines 'Deque constructors' (Select-Constructors $dequeStatements)
Add-DeclarationList $lines 'Dictionary constructors' (Select-Constructors $dictionaryStatements)
Add-DeclarationList $lines 'HashSet constructors' (Select-Constructors $hashSetStatements)

$lines.Add('## Concrete-Only Dictionary API')
$lines.Add('')
$dictionaryExtras = Select-DeclarationsByName -Statements $dictionaryStatements -Names @(
  'First',
  'Last',
  'Count',
  'ResizeBuckets',
  'GetBucketCount',
  'BucketCount'
)
Add-DeclarationList $lines 'Dictionary concrete members not present in IThreadSafeDictionary' $dictionaryExtras

$lines.Add('## Built-In Comparers and Hash Helpers')
$lines.Add('')
Add-DeclarationList $lines 'List comparers' (Select-GlobalFunctions -Statements $listStatements -Names @(
  'IntegerComparer',
  'StringComparer',
  'BooleanComparer',
  'RealComparer'
))
Add-DeclarationList $lines 'HashSet equality comparers' (Select-GlobalFunctions -Statements $hashSetStatements -Names @(
  'IntegerEquals',
  'StringEquals',
  'BooleanEquals',
  'RealEquals'
))
Add-DeclarationList $lines 'HashSet primitive hash functions' (Select-GlobalFunctions -Statements $hashSetStatements -Names @(
  'IntegerHash',
  'StringHash',
  'BooleanHash',
  'RealHash'
))
Add-DeclarationList $lines 'HashFunctions unit' (Select-GlobalFunctions -Statements $hashFunctionStatements -Names @(
  'XXHash32',
  'FNV1aHash',
  'MultiplicativeHash',
  'DefaultHash'
))

$lines.Add('## Locking Notes')
$lines.Add('')
$lines.Add('- Every collection uses one `TCriticalSection` per collection instance.')
$lines.Add('- Public operations are internally synchronized.')
$lines.Add('- Manual `Lock()` returns `ILockToken`, but public methods on the same collection also acquire the same lock.')
$lines.Add('- Do not hold a manual token and then call public methods on the same collection on non-reentrant `TCriticalSection` platforms.')
$lines.Add('- List, HashSet, and Deque iterators hold the lock for the full loop.')
$lines.Add('- Dictionary iterators copy a snapshot, release the lock, then iterate over the snapshot.')
$lines.Add('')
$lines.Add('## Behaviour Notes')
$lines.Add('')
$lines.Add('- `TThreadSafeList` uses direction-aware binary search for `IndexOf` and `Contains` after either ascending or descending `Sort`.')
$lines.Add('- Dictionary default key equality and `ContainsValue` use RTL type-aware default equality comparers.')
$lines.Add('- Dictionary and HashSet collection bulk overloads snapshot the source before mutating the destination, avoiding nested source locks on POSIX.')
$lines.Add('- `TThreadSafeDeque.PushRangeFront` prepends values in input order, so the last input item becomes the front item.')

$outputDir = Split-Path -Parent $OutputPath
if (-not (Test-Path -LiteralPath $outputDir)) {
  New-Item -ItemType Directory -Path $outputDir | Out-Null
}

Set-Content -LiteralPath $OutputPath -Value $lines -Encoding UTF8
Write-Host "Generated $(ConvertTo-RepoPath $OutputPath)"
