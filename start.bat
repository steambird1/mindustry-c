@echo off
pushd %~dp0
start py -m http.server
timeout -t 2 >nul
start http://localhost:8000/main.html
popd
