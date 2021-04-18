library(dplyr)
library(stringr)
library(textclean)


# replaceTranslit = function() {
#   
#   context <- rstudioapi::getActiveDocumentContext()
#   
#   for (sel in context$selection) {
#     
#     txt  <- sel$text
#     
#     newtxt <- translit(txt)
#     
#     rstudioapi::modifyRange(sel$range, as.character(newtxt), context$id)
#     
#   }
# }

bookdown::render_book(input = ".")
bookdown::publish_book(name = "Esna2", account = "shemanefer", server = "bookdown.org")
rmarkdown::find_pandoc(dir = "c://users/klotzd/AppData/local/pandoc/", version = "2.13.0")

fix_translit <- function(x) {
  
  textclean::mgsub_fixed(x, pattern = c("a", "A", "i", "H", "x", "X","S", "D", "T"), c("ʿ","ȝ", "ỉ", "ḥ","ḫ","ẖ","š", "ḏ","ṯ"))
  

}

tmp_theme <- function() bookdown::bs4_book(theme = bookdown::bs4_book_theme(font_scale = .5, 
                                                                            primary = "#9B110E"))
translate <- function(x){
 
    stringr::str_split(x, "\\n") %>% 
    unlist %>% 
    #paste0("| ", .) %>% 
    paste(collapse = "  \n") %>% 
    cat
    
}
translit <- function(x){
  
  fix_translit(x) %>% 
    stringr::str_split(., "\\n") %>% 
    unlist %>% 
    paste0("*", ., "*  ") %>% 
    stringi::stri_replace_all_fixed("*  ", "  *") %>% 
    stringi::stri_replace_all_fixed("*  ", "  *") %>% 
    stringi::stri_replace_all_fixed("*  ", "  *") %>% 
    stringi::stri_replace_all_fixed("*  ", "  *") %>% 
    paste(collapse = "  \n") 
  
  
}

translit("| *qȝ pȝwty.w  *
| *ỉty nb nṯr.w rmṯ  *
| *nḫt wr-pḥty  *
| *smȝ bdš  *
| *ỉr ʿnḏ m ʿȝpp  *
| *ẖnm-Rʿ nb sḫ.t  *")

| *nsw.t-bỉty  *
| *nṯr nṯry ḥqȝ pȝwty.w  *
| *ỉty nb | *nṯr  *.w rmṯ  *
| *nḫt wr-pḥty  *
| *smȝ bdš  *
| *ỉr ʿnḏ m ʿȝpp  *
| *ẖnm-Rʿ nb sḫ.t  *
