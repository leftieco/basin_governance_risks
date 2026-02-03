### ---------------------\\ 
# Script objective:
# Generate function that reflects study bias towards selecting/identifying a manageable number of groundwaterscapes 
### ---------------------\\
library(here); source(here(("scripts/on_button.R")))
###

# import the prototype data, and estimate the number of cluster centers
prototypes = readr::read_rds(here("data/som_files/som_files_full/som1_nrc_22_iter_37.rds"))
prototypes = prototypes$codes[[1]] |> as_tibble()

clut_apriori = NbClust(data = prototypes, 
                      min.nc = 2, max.nc = 30, 
                      method = "ward.D2", 
                      index = "all")

# weighted mean of all clustering suggestions
apriori_df = clut_apriori$Best.nc |> as_tibble() |> t() |> set_colnames(c('nc', 'value')) |> as.data.frame()
apriori_df = apriori_df$nc |> unlist() |> as.vector() |> as.numeric() |> table() |> as.data.frame() |> set_colnames(c('best_nc', 'freq')) 
apriori_df$best_nc = apriori_df$best_nc |> as.character() |> as.numeric()
apriori_df$freq = apriori_df$freq |> as.character() |> as.numeric()

best_nc = trunc(median(apriori_df$best_nc)[1])
#> 8
best_nc = 8

# Import performance metrics of each SOM2 architecture size
df = list.files(here("data/som_files/som2_performance/"), pattern = ".rds", full.names = T) |> 
  map_dfr(readRDS) 
nrow(df)
names(df)[7] = "som_size"
# ! MAKE SURE nrc is named to som_size, wherever it is located in data.table

##
### generate size preference function 
##

triangle_df = tibble(
  som_size = seq(2, 30)) |> 
  mutate(size_preference = dnorm(seq(1, 100, length.out = 29)/100, 0.5, 2))
triangle_df$size_preference_scaled = minmaxnorm(triangle_df$size_preference / sd(triangle_df$size_preference))
plot(triangle_df$size_preference_scaled ~ triangle_df$som_size)

norm_extract = tibble(y = minmaxnorm(dnorm(seq(0, 100, length.out = 58)/100, mean = 0.5, sd = 1)),
                      x = c(2, c(2 + 28 * (seq(0, 100, length.out = 57)/100)))) 
norm_extract$y = as.numeric(norm_extract$y)
norm_extract$x = as.numeric(norm_extract$x) |> round(1)
# plot(minmaxnorm(dnorm(seq(0, 100, length.out = 58)/100, mean = 0.5, sd = 1)) ~
#        c(2, c(2 + 28 * (seq(0, 100, length.out = 57)/100))))

top = norm_extract |> dplyr::filter(x == best_nc) |> pull(y)
triangle_df$size_preference_scaled[triangle_df$size_preference_scaled > top] = top
plot(triangle_df$size_preference_scaled ~ triangle_df$som_size)
triangle_df$size_preference_scaled = 1- minmaxnorm(triangle_df$size_preference_scaled)

plot(triangle_df$size_preference_scaled ~ triangle_df$som_size)

df = merge(x = df, y = triangle_df,
           by.x = "som_size", by.y = "som_size")

df$sizeperception = log(df$som_size)
df$sizeperception_scaled = minmaxnorm(log(df$som_size) / sd(log(df$som_size)))
plot(df$sizeperception_scaled ~ df$som_size)

df$size_bias = minmaxnorm((df$sizeperception_scaled + 3*df$size_preference_scaled))
plot(df$size_bias ~ df$som_size)

df = df |> dplyr::select(c(som_size, iter, size_bias))

write_rds(df, here("data/som2_size_bias.rds"))