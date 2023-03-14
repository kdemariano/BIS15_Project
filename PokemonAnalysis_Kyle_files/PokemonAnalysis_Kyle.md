---
title: "Pokemon_kyle"
output: 
  html_document: 
    keep_md: yes
date: "2023-03-02"
---

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   0.3.4 
## ✔ tibble  3.1.6      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.0 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(ggplot2)
library(dplyr)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```



```r
pokemon<- read_csv("pokemon_tidy.csv")
```

```
## New names:
## Rows: 801 Columns: 44
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (9): primary_attack, secondary_attack, tertiary_attack, capture_rate, c... dbl
## (35): ...1, against_bug, against_dark, against_dragon, against_electric,...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

```r
pokemon %>%  #legendary
  filter(is_legendary==1)
```

```
## # A tibble: 70 × 44
##     ...1 primary_attack secondary_attack tertiary_attack against_bug
##    <dbl> <chr>          <chr>            <chr>                 <dbl>
##  1   144 Pressure       Snow Cloak       <NA>                   0.5 
##  2   145 Pressure       Static           <NA>                   0.5 
##  3   146 Pressure       Flame Body       <NA>                   0.25
##  4   150 Pressure       Unnerve          <NA>                   2   
##  5   151 Synchronize    <NA>             <NA>                   2   
##  6   243 Pressure       Inner Focus      <NA>                   1   
##  7   244 Pressure       Inner Focus      <NA>                   0.5 
##  8   245 Pressure       Inner Focus      <NA>                   1   
##  9   249 Pressure       Multiscale       <NA>                   1   
## 10   250 Pressure       Regenerator      <NA>                   0.25
## # … with 60 more rows, and 39 more variables: against_dark <dbl>,
## #   against_dragon <dbl>, against_electric <dbl>, against_fairy <dbl>,
## #   against_fight <dbl>, against_fire <dbl>, against_flying <dbl>,
## #   against_ghost <dbl>, against_grass <dbl>, against_ground <dbl>,
## #   against_ice <dbl>, against_normal <dbl>, against_poison <dbl>,
## #   against_psychic <dbl>, against_rock <dbl>, against_steel <dbl>,
## #   against_water <dbl>, attack <dbl>, base_egg_steps <dbl>, …
```


```r
pokemon %>% #nonlegendary
  filter(is_legendary==0)
```

```
## # A tibble: 731 × 44
##     ...1 primary_attack secondary_attack tertiary_attack against_bug
##    <dbl> <chr>          <chr>            <chr>                 <dbl>
##  1     1 Overgrow       Chlorophyll      <NA>                   1   
##  2     2 Overgrow       Chlorophyll      <NA>                   1   
##  3     3 Overgrow       Chlorophyll      <NA>                   1   
##  4     4 Blaze          Solar Power      <NA>                   0.5 
##  5     5 Blaze          Solar Power      <NA>                   0.5 
##  6     6 Blaze          Solar Power      <NA>                   0.25
##  7     7 Torrent        Rain Dish        <NA>                   1   
##  8     8 Torrent        Rain Dish        <NA>                   1   
##  9     9 Torrent        Rain Dish        <NA>                   1   
## 10    10 Shield Dust    Run Away         <NA>                   1   
## # … with 721 more rows, and 39 more variables: against_dark <dbl>,
## #   against_dragon <dbl>, against_electric <dbl>, against_fairy <dbl>,
## #   against_fight <dbl>, against_fire <dbl>, against_flying <dbl>,
## #   against_ghost <dbl>, against_grass <dbl>, against_ground <dbl>,
## #   against_ice <dbl>, against_normal <dbl>, against_poison <dbl>,
## #   against_psychic <dbl>, against_rock <dbl>, against_steel <dbl>,
## #   against_water <dbl>, attack <dbl>, base_egg_steps <dbl>, …
```



```r
#Comparing Pokemon by Generation
pokemon %>% 
  group_by(generation) %>% 
  filter(is_legendary=="0") %>% 
  summarize("mean_base_total"=mean(base_total)) %>% 
  ggplot(aes(x=generation,y=mean_base_total, fill=generation))+ geom_col()+ labs(title="Comparing Base Stats by Generation", x="Generation", y="Mean Base Total")
```

![](PokemonAnalysis_Kyle_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
#Table of Stats by generation, no legendaries
pokemon %>%
  group_by(generation) %>% 
  filter(is_legendary=="0") %>% 
  summarize(mean_base_total=mean(base_total),
            mean_attack=mean(attack),
            mean_sp_attack=mean(sp_attack),
            mean_defense=mean(defense),
            mean_sp_defense=mean(sp_defense),
            mean_speed=mean(speed),
            mean_hp=mean(hp))
```

```
## # A tibble: 7 × 8
##   generation mean_base_total mean_attack mean_sp_attack mean_defense
##        <dbl>           <dbl>       <dbl>          <dbl>        <dbl>
## 1          1            409.        73.5           67.4         69.4
## 2          2            400.        68.1           63.2         70.0
## 3          3            400.        74.1           67.4         68.8
## 4          4            427.        79.2           69.2         72.4
## 5          5            410.        77.6           66           70  
## 6          6            416.        71.5           71.2         71.1
## 7          7            419.        80.2           68.2         74.5
## # … with 3 more variables: mean_sp_defense <dbl>, mean_speed <dbl>,
## #   mean_hp <dbl>
```



```r
#Comparing Dual Types vs Monotypes
pokemon %>% 
  filter(is_legendary=="0") %>% 
  mutate(second_type = ifelse(type2 != "NA","Yes","No")) %>% 
  group_by(second_type) %>% 
  summarize(mean_base_total=mean(base_total)) %>% 
  ggplot(aes(x=second_type,y=mean_base_total, fill= second_type))+geom_col()+ labs(title = "Comparing Monotypes vs Dual Types", x="Second Type", y="Mean Base Total")
```

![](PokemonAnalysis_Kyle_files/figure-html/unnamed-chunk-7-1.png)<!-- -->




```r
#Best Starter for Region 1
pokemon %>% 
 filter(name=="Bulbasaur" | name=="Charmander" | name=="Squirtle") %>% 
  select(name, base_total,attack, sp_attack, defense, sp_defense, speed, hp)
```

```
## # A tibble: 3 × 8
##   name       base_total attack sp_attack defense sp_defense speed    hp
##   <chr>           <dbl>  <dbl>     <dbl>   <dbl>      <dbl> <dbl> <dbl>
## 1 Bulbasaur         318     49        65      49         65    45    45
## 2 Charmander        309     52        60      43         50    65    39
## 3 Squirtle          314     48        50      65         64    43    44
```


```r
#Best Evolved Starter for Region 1
pokemon %>% 
 filter(name=="Venusaur" | name=="Charizard" | name=="Blastoise") %>% 
  select(name, base_total, attack, sp_attack, defense, sp_defense, speed, hp)
```

```
## # A tibble: 3 × 8
##   name      base_total attack sp_attack defense sp_defense speed    hp
##   <chr>          <dbl>  <dbl>     <dbl>   <dbl>      <dbl> <dbl> <dbl>
## 1 Venusaur         625    100       122     123        120    80    80
## 2 Charizard        634    104       159      78        115   100    78
## 3 Blastoise        630    103       135     120        115    78    79
```

```r
pokemon2<- pokemon%>% 
  group_by(is_legendary) %>% 
  summarize(mean_base_total=mean(base_total),
            mean_attack=mean(attack),
            mean_sp_attack=mean(sp_attack),
            mean_defense=mean(defense),
            mean_sp_defense=mean(sp_defense),
            mean_speed=mean(speed),
            mean_hp=mean(hp))
#Table of comparing legendary pokemon to non legendary
pokemon2 %>% 
  mutate(legendary=ifelse(is_legendary== 1,"Yes","No")) %>% 
  select(!is_legendary)
```

```
## # A tibble: 2 × 8
##   mean_base_total mean_attack mean_sp_attack mean_defense mean_sp_defense
##             <dbl>       <dbl>          <dbl>        <dbl>           <dbl>
## 1            410.        74.8           67.2         70.5            67.9
## 2            615.       109.           114.          99.4           102. 
## # … with 3 more variables: mean_speed <dbl>, mean_hp <dbl>, legendary <chr>
```

