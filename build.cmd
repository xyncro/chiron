@echo off

if not exist .paket\paket.exe ( 
  .paket\paket.bootstrapper.exe
)

.paket\paket.exe restore

set encoding=utf-8
packages\FAKE\tools\FAKE.exe build.fsx %*
