This is a trivial implementation of a POI warner. It uses gpsd to obtain your
current position and alerts you when you approach a POI. The POIs are stored in
one or more IGO8 CSV files or in SQLite databases. This project depends 
on [hsgps](https://github.com/greg42/hsgps).

Sample usage: `hspoi 127.0.0.1 2947 'echo POI %s in %d meters' my_pois.csv`

For making use of SQLite databases, please invoke `hspoi` as follows:
`hspoi 127.0.0.1 2947 'echo POI %s in %d meters' 'poi.sqlite3:tbl:lat:lon:type:additionalWhereClause'`
Instead of a CSV file, you supply the name of a SQLite database, followed by the
following colon-separated values:
* `tbl` - The name of the table that contains the POI information
* `lat` - The name of the column in `tbl` that contains the latitude of the POI
* `lon` - The name of the column in `tbl` that contains the longitude of the POI
* `type` - The name of the column in `tbl` that contains a textual description
  of the POI
* `additionalWhereClause` - An additional `WHERE` clause for selecting from
  `tbl`

In any case (be it CSV or SQLite databases), you can supply more than one
sources if you like. You can also mix CSV and SQLite databases.

Currently, the distances to the POIs that will trigger a warning are hard-coded
(1000m, 500m, 300m, 100m), but this is trivial to change.
