This is a trivial implementation of a POI warner. It uses gpsd to obtain your
current position and alerts you when you approach a POI. The POIs are stored in
one or more IGO8 CSV files. This project depends on [hsgps](https://github.com/greg42/hsgps).

Sample usage: `hspoi 127.0.0.1 2947 'echo POI %s in %d meters' my_pois.csv`
