# NOT actually a dockerfile yet

sudo yum install libcurl-devel openssl-devel libxml2-devel udunits2-devel proj-devel geos geos-devel netcdf-devel wget postgresql postgresql-devel SFCGAL-devel proj-nad proj-epsg

cd /tmp
wget http://s3.amazonaws.com/etc-data.koordinates.com/gdal-travisci/install-libkml-r864-64bit.tar.gz
tar xzf install-libkml-r864-64bit.tar.gz
sudo cp -r install-libkml/include/* /usr/local/include
sudo cp -r install-libkml/lib/* /usr/local/lib
sudo ldconfig

#download GDAL
wget http://download.osgeo.org/gdal/2.2.3/gdal-2.2.3.tar.gz

#Untar
tar xzf gdal-2.2.3.tar.gz
cd gdal-2.2.3

#Compile from source --prefix=/usr/
./configure --with-libkml
sudo make
sudo make install
sudo ldconfig

#I would up having to modify /usr/lib64/R/etc/ldpaths to add /usr/local/lib to R_JAVA_LD_LIBRARY_PATH
R -e "install.packages('ALEPlot',repos = 'https://cran.wustl.edu',dependencies = T)"



