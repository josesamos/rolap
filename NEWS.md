# rolap 2.5.1
* Include scd structure for star database. 
* New test data for scd.
* Modify the cph field of the description.

# rolap 2.5.0
* Geographic information query functions: `as_geolayer()`, `get_layer()`, `get_variables()`, `set_variables()`, `as_GeoPackage()`.
* Functions to define geoattributes: `define_geoattribute()`, `check_geoattribute_geometry()`, `get_geoattributes()`, `get_geoattribute_geometries()`.
* Geographic information processing functions: `coordinates_to_point()`, `get_layer_geometry()`, `summarize_layer()`, `get_point_geometry()`.

# rolap 2.4.0
* Functions to query a star database: `star_query()`, `select_fact()`, `select_dimension()`, `filter_dimension()` and `run_query()`.
* Functions to deploy a star database in one or more relational databases: `deploy()`, `cancel_deployment()`, `get_deployment_names()`, `load_star_database()`.
* New functions: `get_star_database()`, `as_rdb()`, `draw_tables()`.
* Vignette about star database deployment process.
* Vignette about star database export features.
* Vignette about multidimensional queries.
* Include new items in the README file.
* US census flat table dataset.
* Fix `snake_case()` register refresh operation for constellation case.
* Fix `constellation()` refresh structure integration.

# rolap 2.3.0
* Change star database list as parameter in `constellation()` function.
* Fix `replace_empty_values()` with spaces at the beginning or end of non-empty strings.
* Star databases data incremental refresh functionality: `update_according_to()`, `get_transformation_code()`, `get_new_dimension_instances()` and `incremental_refresh()` among other functions.
* Vignette about star databases incremental refresh.
* Fix new problem building macos-latest (release) with input functions from file or database in vignettes.

# rolap 2.2.0
* Vignette about obtaining and transforming flat tables.
* Definition of flat_table class with functions to transform an integrate data.
* Vignette about integration of dimension instances.
* Functions to query and modify instances of dimensions.
* Definition of a common data model for star databases and constellations.
* Vignette about `md` and `rolap` packages.

# rolap 2.1.0
* Definition of role-playing and role dimensions (with explanatory vignette): `role_playing_dimension()` and `get_role_playing_dimension_names()` functions.
* New functions to query and rename attributes in dimensions and measures in facts: `get_attribute_names()`, `set_attribute_names()`, `get_measure_names()` and `set_measure_names()`.
* Include detailed validation error messages.

# rolap 2.0.0
* New version from the experience in the `starschemar` package.
* Initial functionality for defining star databases and constellations.
* Export of the data model to RDBMS.
