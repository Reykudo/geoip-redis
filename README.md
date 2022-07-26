# geoip-redis
2-part package for loading [GeoLite2 Database](https://dev.maxmind.com/geoip/geolite2-free-geolocation-data?lang=en) to redis and working with it from your application.

## app
Contains logic abot uploading existing GeoLite2 database to redis
## lib
Contains types + lookup functions

## Usage
1. login [here](https://dev.maxmind.com/geoip/geolite2-free-geolocation-data?lang=en)
2. download database (you need 2 files: GeoLite2-City-Blocks-IPv4.csv and GeoLite2-City-Locations-<lang>.csv)
3. install redis
4. run `geoip-redis-exe -r 'redis://user@localhost:6379/1' -b './data/GeoLite2-City-Blocks-IPv4.csv' -l './data/GeoLite2-City-Locations-ru.csv' -d 'geoip_ipv4'` on host where you need database
5. run your app (which use `./lib` i guess)