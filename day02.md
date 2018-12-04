day\_02
================
A Gruer
4 December 2018

## Part 1

``` r
test_input <- c("abcdef",
"bababc",
"abbcde",
"abcccd",
"aabcdd",
"abcdee",
"ababab")

checksum <- function(input) {
 str_split(input,boundary("character")) %>% 
  purrr::map(sort) %>% 
  purrr::map(rle) %>% 
  purrr::map("lengths") %>% 
  purrr::map_dfr( function(run_lengths){
      twos <-  any(which(run_lengths == 2) !=0) 
      threes <- any(which(run_lengths == 3) !=0)
      tibble(twos_cnt = as.numeric(twos), threes_cnt = as.numeric(threes))
      }) %>% 
  colSums() %>% 
  prod()
}

 
expect_identical(checksum(test_input), 12)

input <- readLines("day02_data.txt")
checksum(input)
```

    ## [1] 6422

## Part 2

``` r
test_input <- c("abcde",
  "fghij",
  "klmno",
  "pqrst",
  "fguij",
  "axcye",
  "wvxyz")

split_to_chars <- function(input){
  str_split(input,boundary("character")) 
}

find_match <- function(input){
    test_chars <- split_to_chars(input)
    
    
    
    
    compare <- reduce(test_chars, function(acc,e){
          acc$matches = c(acc$matches,list(map(acc$compare, ~ . != e)))
          acc
    }, .init = list(matches = list(logical()), compare = test_chars))
    
    
    matches <- test_chars[compare$matches[-1] %>% map(~ map_dbl(., ~sum(.) )) %>%
      map_lgl(~ any( . == 1) )
      ]
  
    matches[[1]][matches[[1]] == matches[[2]]] %>% 
       paste0(collapse="")
      
}

expect_identical(find_match(test_input), "fgij")

find_match(input)
```

    ## [1] "qcslyvphgkrmdawljuefotxbh"
