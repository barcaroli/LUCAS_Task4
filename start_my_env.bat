@echo off
setlocal

REM === Imposta la cartella dove vuoi salvare l’ambiente virtuale
set ENV_DIR=C:\PythonEnv\my_env

REM === Verifica se l’ambiente esiste, altrimenti crealo
if not exist "%ENV_DIR%\Scripts\activate.bat" (
    echo Creazione dell'ambiente virtuale in: %ENV_DIR%
    python -m venv "%ENV_DIR%"
    if errorlevel 1 (
        echo Errore nella creazione dell'ambiente virtuale.
        pause
        exit /b 1
    )
)

REM === Attiva l'ambiente virtuale
call "%ENV_DIR%\Scripts\activate.bat"

REM === Mostra info
echo Ambiente virtuale attivato in: %ENV_DIR%
echo Python path: %~dp0
python --version
pip list

REM === Avvia prompt interattivo
cmd

endlocal
