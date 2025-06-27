@echo off
setlocal

:: Set working directory
set WORKSPACE=%CD%\batteries

:: Clone the batteries repo
git clone --branch main https://github.com/leanprover-community/batteries.git "%WORKSPACE%"

:: Reset to specific commit
cd /d "%WORKSPACE%"
git reset --hard 80520e5834d0d9a2446cb88ea3d2a38a94d2e143

:: Copy files (assumes they are in the same directory as this .bat file)
copy "%~dp0batteriesLakefile.toml" "%WORKSPACE%\lakefile.toml"
copy "%~dp0ProjectFormat.lean" "%WORKSPACE%\ProjectFormat.lean"

:: Add Lean to PATH for this session (adjust version if needed)
set LEAN_BIN=%USERPROFILE%\.elan\toolchains\leanprover--lean4---v4.17.0-rc1\bin
set PATH=%LEAN_BIN%;%PATH%

:: Run lake commands
cd /d "%WORKSPACE%"
lake update
lake build

endlocal