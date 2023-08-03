get_onedrive <- function (team="Martin Pastoors", site="FLYSHOOT - General") {
  
  if (Sys.info()['sysname'] == 'Windows') {
    
    # set onedrive directory
    if(dir.exists(file.path(Sys.getenv('USERPROFILE'), team, site))) {
      onedrive <- file.path(Sys.getenv('USERPROFILE'), team, site)   
    } else if(dir.exists(file.path('C:/DATA/PFA', team, site))) {
      onedrive <- file.path('C:/DATA/PFA', team, site)
    } else if(dir.exists(file.path('D:/DATA/PFA', team, site))) {
      onedrive <- file.path('D:/DATA/PFA', team, site)
    } else {
      stop("Onedrive directory not found")
    }
  }
  
  return(onedrive)
}
