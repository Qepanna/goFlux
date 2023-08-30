# Utility functions for the GoFluxYourself package

#' Move files in folder to another folder
#'
#' Wrapper function for moving all files contained in a folder to another folder.
#'
#' @param in_path a character vector of full path name. Input folder containing files to move
#' @param out_path a character vector of full path name. Output folder to move files into
#' @param pattern an optional regular expression. Only file names which match
#'                the regular expression will be returned.
#' @param recursive logical. Should directories be deleted recursively?
#' @return None
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' # create a dummy in_path folder
#' if(dir.exists("in_path") == FALSE){dir.create("in_path")}
#'
#' # write a dummy file
#' write("dummy", file = "in_path/dummy.txt")
#'
#' # move dummy file to out_path
#' # when recursive = TRUE, folder "in_path" is erased
#' mv.dir("in_path", "out_path", pattern = ".txt", recursive = TRUE)
#'
#' # erase out_path and dummy file
#' unlink("out_path", recursive = TRUE)
#'
#' @export
mv.dir <- function(in_path, out_path, pattern = NULL, recursive = TRUE) {
  if(dir.exists(out_path) == FALSE){dir.create(out_path)}

  in_files <- list.files(in_path, pattern = pattern, recursive = TRUE)

  file.copy(from = paste(in_path, in_files, sep = "/"),
            to = out_path, recursive = TRUE)

  if (recursive == FALSE) {
    unlink(paste(in_path, in_files, sep = "/"), recursive = FALSE)
  }
  if (recursive == TRUE) {
    unlink(paste(in_path, sep = "/"), recursive = TRUE)
  }

}
