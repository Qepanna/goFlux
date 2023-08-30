#' Suppress messages
#'
#' Function for suppressing messages from other functions.
#'
#' @param messages logical.
#' @param ... objects evaluated by eval()
#' @return removes messages from evaluated statements
#' @keywords internal
supress_message <- function (..., messages = FALSE) {
  out <- if (messages)
    eval(...)
  else suppressMessages(eval(...))
  out
}
