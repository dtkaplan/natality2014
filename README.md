# natality2014 package for R

Data from the CDC on all 4 million births in the US in 2014

These data provide diverse opportunities for teaching statistics using short, genuine research projects. See the notes in `vignettes/intro_stats.html`.

The package provides three data files via `data()`:

1. `Natality_2014_100k` --- a random sample of size 100k from the complete set of 4 million CDC cases.
2. `Natality_2014_10k` --- a random subsample of size 10k from (1).
3. `Natality_2014_1k` --- a random subsample of size 1k from (2).

These three files are small enough that loading the package is very fast. Should you want larger data sets, you can access them as described under `help("Larger_natality_data_files")`. These larger files are so big that making them available via `data()` would dramatically slow attaching the package.

See the documentation for `Natality_2014_100k` for a description of the variables, etc.
