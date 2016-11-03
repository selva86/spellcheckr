## spellcheckr
Corrects the spelling of a given word in English using a modification of Peter Norvig's spell correct algorithm but is enhanced to handle up to three edits. 
The algorithm tries to find the spelling with maximum probability of intended correction out of all possible candidate corrections derived from the original word.

#### How to install?
    # Install Dev Version on GitHub
    install.packages("devtools")  # installs devtools pkg
    devtools::install_github("selva86/spellcheckr")

    # Install CRAN version
    install.packages("spellcheckr")

#### Examples
    library(spellcheckr)

##### 1. spellcorrect a word
    correct("misteke")
    #> [1] "mistake"
    correct("objet")
    #> [1] "object"

##### 2. spellcorrect a collection of words
    sapply(c("The", "audiance", "apprecitad", "my", "singyng"), correct)
    #> The      audiance    apprecitad            my       singyng 
    #> "the"    "audience" "appreciated"          "my"     "singing" 

##### 3. spellcorrect a sentence
    sent <- c("The audiance apprecitad my singyng")
    wrds <- unlist(stringr::str_split(sent, " "))  # `stringr` package is needed for this.
    wrds_out <- sapply(wrds, correct)
    paste(wrds_out, collapse=" ")
    #> [1] "the audience appreciated my singing"
