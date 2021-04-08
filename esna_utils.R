library(dplyr)
library(stringr)
library(textclean)

fix_translit <- function(x) {
  
  textclean::mgsub_fixed(x, pattern = c("a", "A", "i", "H", "x", "X","S", "D", "T"), c("ʿ","ȝ", "ỉ", "ḥ","ḫ","ẖ","š", "ḏ","ṯ"))
  

}

translit <- function(x){
  
  fix_translit(x) %>% 
    stringr::str_split(., "\\n") %>% 
    unlist %>% 
    paste0("| *", ., "*  ") %>% 
    stringi::stri_replace_all_fixed("*  ", "  *") %>% 
    stringi::stri_replace_all_fixed("*  ", "  *") %>% 
    stringi::stri_replace_all_fixed("*  ", "  *") %>% 
    paste(collapse = "\n") %>% 
    cat
  
  
}

translit("nsw.t-bity
nTr nTry HqA pAwty.w
ity nb nTr.w rmT
nxt wr-pHty
smA bdS
ir anD m aApp
Xnm-Ra nb sx.t")
