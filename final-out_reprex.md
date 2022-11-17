``` r
x <- sf::read_sf(here::here("final-out.geojson"))
cat("dims:", paste(dim(x), collapse = ","), "\n")
#> dims: 197,281
plot(x['prog_name'])
```

![](https://i.imgur.com/aMJVQlN.png)

<sup>Created on 2022-11-17 by the [reprex package](https://reprex.tidyverse.org) (v2.0.1)</sup>
