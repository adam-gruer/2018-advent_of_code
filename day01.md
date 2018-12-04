day\_01
================
A Gruer
4 December 2018

``` r
 input <- read_lines("day01_data.txt") %>% as.numeric()
```

Frequency

``` r
sum(input)
```

    ## [1] 531

Duplicated feqrency

``` r
acc <- numeric()
while(TRUE){
 acc <-   c(acc,input)
 c <-  cumsum(acc)  
 dup <- anyDuplicated(c) 
 if(dup) break
}

c[dup]
```

    ## [1] 76787
