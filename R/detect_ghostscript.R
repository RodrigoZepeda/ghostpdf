#' Detect ghostscript installation
#'
#' @param quiet Do not show messages
#' @return path to ghostscript.
#' @importFrom glue glue
#' @importFrom rlang is_empty

detect_ghostscript <- function(quiet = F){

  #Look for ghostscript
  if (.Platform$OS.type == "unix"){

    #In Unix-like platforms ghostscript is written as gs
    gs_dir    <- system2("which", "gs", stdout=TRUE)

  } else {

    #In windows two possible ghostscript versions
    gs_dir_32 <- system2("which", "gswin32c", stdout=TRUE)
    gs_dir_64 <- system2("which", "gswin64c", stdout=TRUE)

    #Version 64
    if (rlang::is_empty(gs_dir_32) & !rlang::is_empty(gs_dir_64)){
      gs_dir <- gs_dir_64

    #Version 32
    } else if (!rlang::is_empty(gs_dir_32) & rlang::is_empty(gs_dir_64)){
      gs_dir <- gs_dir_32

    #Both versions use 64
    } else if (!rlang::is_empty(gs_dir_32) & !rlang::is_empty(gs_dir_64)){
      gs_dir <- gs_dir_64

    #No GS
    } else {
      gs_dir <- character(0)
    }
  }

  #Check if no ghostscript found
  if (rlang::is_empty(gs_dir)){
    gs_version <- NULL
    stop(paste("Ghostscript not found. Please install as following:",
               " > [Windows] go to https://www.ghostscript.com/releases/gsdnld.html",
               " > [OSX] brew install ghostscript",
               " > [Ubuntu/Debian] sudo apt-get install ghostscript",
               " > [CentOS/Fedora] sudo yum install ghostscript",
               sep = "\n"))
  } else {
    gs_version <- system2(gs_dir, "--version", stdout=TRUE)
    if (!quiet){
      message(glue::glue("Ghostscript version {gs_version} found at {gs_dir}"))
    }
  }

  return(list("gs" = gs_dir, "version" = gs_version))
}
