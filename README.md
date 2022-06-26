
# trophic
-----

The trophic package for R allows the estimation of trophic niche breadth and overlap and their confidence intervals at a defined level. It is based on the proposed analysis for anuran trophic niche of Herrera-Lopera et al. (Salamandra, in press).

-----

## Installation

You can install the development version of trophic from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MarioHerreraLopera/trophic")

library(trophic)
```
-----

## Citation
Please cite trophic (and other R packages it depends on) by using:

```r
citation("trophic")
citation("vegetarian")
citation("spaa")
```

-----

## Example

The following are examples of how to estimate trophic niche breadth and trophic niche overlap for two fictitious species using the "trophic" package. The niche breadth estimation functions are based on the 0, 1 and 2 orders of the Hill series, while the package allows estimation of niche overlap using both traditional measures and those derived from the Hill series. For more information please see Herrera-Lopera et al. (Salamandra Journal, in press).

### Two fictitious species: Two matrices of abundances of prey consumed by individuals are generated, where the rows represent the prey and the columns represent each individual in the sample. In this case, we generated two matrices for two fictitious species with 10 prey and 10 individuals each.

``` r
library(trophic)

species_A <- matrix(sample(0:5, 100, replace = T), nrow = 10, ncol = 10) ## First species

species_B <- matrix(sample(0:7, 100, replace = T), nrow = 10, ncol = 10) ## Second species

species_A

species_B
```

### Creating input matrices: The analyses require generating n times resampled matrices (iterations), in which, for each iteration, an individual of the species is selected and a column is added to the input matrix. If it is not desired to iterate (add columns to the original matrix), confidence intervals can be estimated using the original matrix, in this case it = ncol(x).

``` r
spA.ent <- n.matrix(species_A, it = 100) ## Entrace matrix for species A
spB.ent <- n.matrix(species_A, it = 100) ## Entrace matrix for species B

spA.ent
spB.ent
```

### Calculating trophic niche breadth: For both species, it is important to note that the number of iterations MUST be equal to that used for the input matrices (n.matrix function). Three diversity values are returned, following Hill's series classical interpretation, where, in this case 0 = prey richness, 1 = effective number of equally common prey, and 2 = effective number of dominant prey. For more information see Herrera-Lopera et al. (Salamandra, in press.).

``` r
n.breadth(spA.ent, it = 100) ## Niche breadth for species A
n.breadth(spB.ent, it = 100) ## Niche breadth for species B
```

### Calculating trophic niche overlap: The package presents two functions for estimating overlap between pairs of species. The first (n.overlap), uses traditional overlap measures (i.e. Pianka, Morisita and Renkonen = Schoener). The second (n.overlap.hill) uses the inverse of Jost's (2007) turnover and returns trophic niche overlap in orders 0, 1 and 2 (see Herrera-Lopera et al. Salamandra, in press, for interpretation). For both functions, it is REQUIRED that the number of iterations is equal for both species and that it is also the same for the overlap function.

``` r
n.overlap(spA.ent, spA.ent, it = 100, method = "pianka") ## Niche overlap for species A and B, using Pianka measure

n.overlap.hill(sp1.ent, sp2.ent, it = 100, order = 1) ## Niche overlap for species A and B, using order 1
```

### For additional information contact mario.herreralopera@gmail.com 
