# Message to Morse Signal Transformer
library("beepr")

morseEncoder <- function(string) {
  stringVec <- unlist(strsplit(tolower(gsub(" ", "|", string)), ""))
  codeTab <- read.csv("morse.csv", header = FALSE)
  colnames(codeTab) <- c("Symbol", "Code")
  # add symbol for space
  codeTab <- rbind(codeTab, data.frame(Symbol = "|", Code = "|"))
  codeTab$Symbol <- tolower(codeTab$Symbol)
  # transform to code
  codeVec <- codeTab$Code[sapply(stringVec,
                                 function(x) which(x == codeTab$Symbol))]
  # transform code to signal output
  signalBreak <- "_"
  signal <- "="
  signalVec <- paste(sapply(codeVec, function(x) {
    symDecomp <- unlist(strsplit(x, ""))
    sigTrans <- function(signalInput) {
      if (signalInput == "-") {
        return(paste(rep(signal, 3), collapse = ""))
      } else if (signalInput == "|") {
        return(paste(rep(signalBreak, 1), collapse = ""))
        # one bleep
      } else {
        return(rep(signal, 1))
      }
    }
    return(paste(sapply(symDecomp, sigTrans), collapse = signalBreak))
  }), collapse = paste(rep(signalBreak, 3), collapse = ""))
  # output signal
  cat(paste0("Message to encode: ", string, "\n"))
  cat(paste0("Signal complete: ", signalVec, "\n"))
  cat("Signal  dynamic: ")
  audioSignal <- sapply(unlist(strsplit(signalVec, "")), function(x) {
    if (x == "=") {
      cat("=")
      beep(1)
    } else {
      cat("_")
      Sys.sleep(0.3)
    }
    flush.console()
  })
}
morseEncoder("Morse code")

