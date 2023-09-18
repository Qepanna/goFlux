#' Sequence repeat
#'
#' A wrapper function that merges seq() and rep().
#'
#' @param from numerical; inherited from seq().
#' @param by numerical; inherited from seq().
#' @param n.rep numerical; number of time the sequence should be repeated.
#' @param length.seq numerical; inherited from seq(..., length.out = length.seq).
#' @param rep.seq logical; instead of repeating a sequence, (1,2,3,1,2,3),
#'                sequence a repetition (1,1,2,2,3,3). Default is rep.seq = F.
#'
#' @return a numerical sequence
#'
#' @keywords internal
#'
seq.rep <- function(from, by, n.rep, length.seq, rep.seq = F) {

  if (rep.seq == F) {

    sequence <- seq(from = from, by = by, length.out = length.seq)
    out.ls <- list()
    for (i in 1:length(sequence)) {
      out.ls[[i]] <- rep(sequence[i], n.rep) }
    unlist(out.ls)

  } else {

    repetition <- rep(from, n.rep)
    out.ls <- list()
    for (i in 1:length(repetition)) {
      out.ls[[i]] <- seq(from = repetition[i], by = by, length.out = length.seq) }
    unlist(out.ls)

  }
}
