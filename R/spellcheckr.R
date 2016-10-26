#' dict
#'
#' A dataset containing the valid english dictionary words originally obtained from a work of Jane Austen.
#'
#' \itemize{
#'   \item . The actual word
#'   \item N The number of occurences of the word in the document.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dict
#' @usage data(dict)
#' @format A data frame with 245462 rows and 2 columns



# Splits: "Create all combinations of splits for a given word."
splits <- function(wd=NULL){
  if(is.null(wd) | (!is.character(wd))){
    stop("The input should be a character string")
  }
  w <- unlist(str_split(wd, ""))
  len <- length(w)
  out <- vector(mode="list", length=len+1)
  out[[1]] <- c("", paste0(w, collapse=""))  # first pair
  out[[len+1]] <- c(paste0(w, collapse=""), "")  # last pair
  if(length(w) > 1){
    for(i in 1:(len-1)){
      out[[i+1]] <- c(paste0(w[1:i], collapse=""), paste0(w[(i+1):len], collapse=""))
    }
  }
  return(out)
}

# deletes: generate all the possible candidates from a word where 1 character is deleted.
deletes <- function(wd=NULL){
  if(is.null(wd) | (!is.character(wd))){
    stop("The input should be a character string")
  }
  pairs <- splits(wd)
  lapply(pairs, function(x){paste0(c(x[1],
                                     str_split(x[2], "") %>% unlist %>% `[`(-1)), collapse="")
  })
}

# transposes: all possible combination of transposes
# tp: Used inside transposes.
tp <- function(x){
  x2 <- str_split(x[2], "") %>% unlist
  len <- length(x2);
  if(len > 2){
    paste0(c(x[1], x2 %>% `[`(c(2, 1, 3:len))), collapse="")
  }else if(len==2){
    paste0(c(x[1], x2 %>% `[`(c(2, 1))), collapse="")
  }
}
transposes <- function(wd=NULL){
  if(is.null(wd) | (!is.character(wd))){
    stop("The input should be a character string")
  }
  pairs <- splits(wd)
  lapply(pairs[1:(length(pairs)-2)], tp)
}
# transposes("prabhu")


# replaces: all combinations of 1 replacement from alphabets a to z.
rp <- function(x){
  x2 <- str_split(x[2], "") %>% unlist
  len <- length(x2);
  if(len>1){
    p1 <- paste0(x2 %>% `[`(c(2:len)), collapse="")
    p2 <- paste0(letters, p1, sep="")
    return(paste(x[1], p2, sep=""))
  }else{  # when the second part of the pair has only 1 character.
    return(paste(x[1], letters, sep=""))
  }
}

replaces <- function(wd=NULL){
  if(is.null(wd) | (!is.character(wd))){
    stop("The input should be a character string")
  }

  pairs <- splits(wd)
  lapply(pairs[-length(pairs)], rp)
}
# replaces("prabhu")


# inserts: all combinations of 1 insert of alphabets a-z
ins <- function(x){
  x2 <- str_split(x[2], "") %>% unlist
  len <- length(x2);
  p1 <- paste0(x2, collapse="")
  p2 <- paste0(letters, p1, sep="")
  return(paste(x[1], p2, sep=""))
}

inserts <- function(wd=NULL){
  if(is.null(wd) | (!is.character(wd))){
    stop("The input should be a character string")
  }
  pairs <- splits(wd)
  lapply(pairs, ins)
}
# inserts("prabhu")

# edit0: Words that are 0 edit away
edit0 <- function(wd=NULL){
  return(wd)
}

# edit1: Words that are 1 edit away
edit1 <- function(wd=NULL){
  if(is.null(wd) | (!is.character(wd))){
    stop("The input should be a character string")
  }

  pairs <- splits(wd)
  deletes <- deletes(wd)  # all possible 1 char deletes
  transposes <- transposes(wd)  # all possible transposes
  replaces <- replaces(wd)  # all possible replaces
  inserts <- inserts(wd)  # all possible inserts.
  return(list(deletes, transposes, replaces, inserts) %>% unlist %>% unique)
}

# edit2: Words that are 2 edits away
edit2 <- function(wd=NULL){
  out <- character()
  edits1 <- edit1(wd)
  for (e in edits1){
    out <- c(out, edit1(e))
  }
  return(unique(out))
}

# edit3 : words that are 3 edits away. Time consuming.
edit3 <- function(wd=NULL){
  edits3 <- character()
  edits2 <- character()
  edits1 <- edit1(wd)
  for (e1 in edits1){
    edits2 <- c(edits2, edit1(e1))
  }

  for (e2 in edits2){
    edits3 <- c(edits3, edit1(e2))
  }
  return(edits3)
}




## Spell correct
#' @title correct
#' @description Correct the spelling of a given word in the english language.
#' @details This is based on Peter Norvig's spell correct algorithm \url{http://norvig.com/spell.py}.
#' But this one is modified to handle upto three edits.
#' @author Selva Prabhakaran \email{selva86@@gmail.com}
#' @export correct
#' @param wd Character. The word to be spell corrected.
#' @param maxedit Integer. The maximum number of edits allowed to reach the correct word. Max allowed is 3.
#' @param dictionary Character Vector of eligible words to be considered. Repeated words will get more weightage.
#' @return The corrected word.
#' @examples
#' data(dict)
#' correct("scaret")
#' correct("beliebe")
#' sapply(c("audiance", "charaktar"), correct)


correct <- function(wd=NULL, maxedit=2, dictionary=NULL){
  if(!is.null(dictionary)){
    dict <- as.data.table(table(unlist(grep('[a-z]+', tolower(dictionary), value = T))))
    setnames(dict, c(".", "N"))
    setkey(dict, ".")
  }

  known <- function(wds){
    wds[wds %in% dict$.]
  }

  k <- known(edit0(wd))
  if(NROW(k) == 0){
    k <- known(edit1(wd))
  }

  if(NROW(k) == 0){
    k <- known(edit2(wd))
  }

  if(maxedit>2){
    if(NROW(k) == 0){
      k <- known(edit3(wd))
    }
  }

  if(NROW(k) == 0){
    return(wd)
  }
  j <- which.max(unlist(dict[k, c(2), with=F]))
  return(k[j])
}

