setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/ALPHAEARTH/micro-pipelines/profili_predicted_app")
# deploy.R — pubblicazione su shinyapps.io
# Sostituisci con le tue credenziali (Account → Tokens → Add Token)
#install.packages('rsconnect')
rsconnect::setAccountInfo(name='giulio-barcaroli',
                          token='D5F027BA1DAA783DE894C9FAD1ACC7F6',
                          secret='4SqitzFEe42DJVww0a7xf7kdnmhNTS1qQdjOfkQK')
rsconnect::deployApp()  # esegui dalla cartella contenente app.R

rsconnect::showLogs(appName = "profili_predicted_app",
                    account = "giulio-barcaroli",
                    streaming = FALSE)
rsconnect::deployApp(
  appName = "profili_predicted_app",
  account = "giulio-barcaroli",
  forceUpdate = TRUE,
  launch.browser = TRUE
)
