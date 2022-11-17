
x <- sf::read_sf(here::here("final-out.geojson"))
cat("dims:", paste(dim(x), collapse = ","), "\n")
plot(x['prog_name'])