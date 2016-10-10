# Clancy-Functions
Useful R functions for Monte Carlo simulations. Functions included are

- `roll.dice`
- `deal.cards`
- `draw.urn`
- `poisson.process`
- `bernoulli.process`

Each function has a help page (e.g. view the `roll.dice` help page with `?roll.dice`). Note that functions will take advantge of multiple cores by default. For small runs (n.iter < 1e4), this may take slightly longer than forcing the function to use a single core.

For installation and loading, run

```
install.packages("devtools")
library(devtools)
install_github("David-Statistics/Clancy-Functions")
library(Clancy.Functions)
```
