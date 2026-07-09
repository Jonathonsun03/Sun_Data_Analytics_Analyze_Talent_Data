@echo off
setlocal

rem Windows entrypoint for Batch API title classification.
rem Run from anywhere inside this repository. Requires WSL/bash for the Linux runner.

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..\..\..\") do set "REPO_ROOT=%%~fI"

pushd "%REPO_ROOT%" >nul
for /f "usebackq delims=" %%I in (`wsl wslpath -a "%REPO_ROOT%"`) do set "WSL_REPO_ROOT=%%I"
wsl bash -lc "cd '%WSL_REPO_ROOT%' && bin/linux/classification/run_title_classification_batch.sh %*"
set "EXIT_CODE=%ERRORLEVEL%"
popd >nul

exit /b %EXIT_CODE%
