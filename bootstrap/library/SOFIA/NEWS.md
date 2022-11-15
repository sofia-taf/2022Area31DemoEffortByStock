# SOFIA 2.1.1 (2022-11-15)

* Removed dependency on areaplot package.




# SOFIA 2.1.0 (2022-11-11)

* Added function addIndex() to add index column to catch data.

* Added plotCat() 'type' option "prop" to draw a proportional area plot. Dropped
  legacy support for 'type' option "all".

* Improved addDriors() so it passes 'shape_prior' and 'growth_rate_prior_cv' to
  format_driors().




# SOFIA 2.0.0 (2022-08-08)

* Added functions gitRepos(), gitClone(), and gitCloneAll() to list and clone
  GitHub repositories.

* Dropped legacy support for 'stocks.combined' in addDriors() and addEffort().
  Use 'same.priors' and 'same.effort' instead.

* Added errors and warnings if stock names in catch/effort/priors don't match.

* Simplified addDriors() so it requires only the 'stock' name, not the 'taxa'
  name. These names used to be identical, but only 'stock' will be used from now
  on.

* Improved plotCat() so it treats the first two columns of the input data frame
  as stock and year.

* Renamed plotCat() 'type' options to "count" and "stock".




# SOFIA 1.2.1 (2022-05-21)

* Renamed argument 'stocks.combined' to 'same.priors' in addDriors().

* Renamed argument 'stocks.combined' to 'same.effort' in addEffort().

* The old argument name 'stocks.combined' is still supported in both addDriors()
  and addEffort() for backward compatibility. The new argument names
  'same.priors' and 'same.effort' should be used in new SOFIA scripts.




# SOFIA 1.2.0 (2022-02-25)

* Added function convertData() to convert primary data to combined data.

* Improved groupData() so it exits gracefully when no CSV files are found.
  Furthermore, it always creates a new set of empty subdirectories for the
  output before copying files.




# SOFIA 1.1.0 (2022-02-20)

* Added function groupData() to group primary data in subdirectories.




# SOFIA 1.0.3 (2022-02-14)

* Improved addDriors() so effort data are passed to format_driors().




# SOFIA 1.0.2 (2022-01-24)

* Initial release.
