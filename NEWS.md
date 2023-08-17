# rolap (development version)
* Functions to update instances in dimensions: `get_similar_attribute_values()`, `replace_attribute_values()`, and `group_dimension_instances()`.
* Common data model for star_databases and constellations for future operations.
* Vignette about `md` and `rolap` packages.

# rolap 2.1.0
* Definition of role-playing and role dimensions (with explanatory vignette): `role_playing_dimension()` and `get_role_playing_dimension_names()` functions.
* New functions to query and rename attributes in dimensions and measures in facts: `get_attribute_names()`, `set_attribute_names()`, `get_measure_names()` and `set_measure_names()`.
* Include detailed validation error messages.

# rolap 2.0.0
* New version from the experience in the `starschemar` package.
* Initial functionality for defining star databases and constellations.
* Export of the data model to RDBMS.
