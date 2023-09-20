## -----------------------------------------------------------------------
# 
# Colleen Leonard, Northwestern University 
# June 2023
# 
# ENVIRONMENTAL DATA EXTRACTION 
# PRECIPITATION- MONTHLY- CHIRPS- https://docs.digitalearthafrica.org/en/latest/data_specs/CHIRPS_specs.html?highlight=rainfall%20chirps%20dataset
## -----------------------------------------------------------------------


#Load Packages
library(chirps)
library(sf)


#Download CHIRPS data
link <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_monthly/tifs/"
url <- paste0(link, "chirps-v2.0.2009.12.tif.gz") #Update to appropriate month and year

#2009
download.file(url, "chirps-v2.0.2009.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2009.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2009.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2009.12.tif.gz", remove = FALSE)

#2010
download.file(url, "chirps-v2.0.2010.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.06.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2010.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2010.12.tif.gz", remove = FALSE)

#2011
url <- paste0(link, "chirps-v2.0.2011.07.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2011.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.06.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.08.tif.gz", remove = FALSE, overwrite= TRUE)

download.file(url, "chirps-v2.0.2011.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.09.tif.gz", remove = FALSE, overwrite= TRUE)

download.file(url, "chirps-v2.0.2011.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.10.tif.gz", remove = FALSE, overwrite= TRUE)

download.file(url, "chirps-v2.0.2011.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2011.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2011.12.tif.gz", remove = FALSE)

#2012
url <- paste0(link, "chirps-v2.0.2012.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2012.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.06.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2012.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2012.12.tif.gz", remove = FALSE)

#2013
url <- paste0(link, "chirps-v2.0.2013.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2013.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.06.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2013.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2013.12.tif.gz", remove = FALSE)

#2014
url <- paste0(link, "chirps-v2.0.2014.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2014.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.06.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2014.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2014.12.tif.gz", remove = FALSE)

#2015
url <- paste0(link, "chirps-v2.0.2015.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2015.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.06.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2015.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2015.12.tif.gz", remove = FALSE)

#2016
url <- paste0(link, "chirps-v2.0.2016.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2016.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.06.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2016.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2016.12.tif.gz", remove = FALSE)

#2017
url <- paste0(link, "chirps-v2.0.2017.08.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2017.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.06.tif.gz", remove = FALSE, )

download.file(url, "chirps-v2.0.2017.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2017.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2017.12.tif.gz", remove = FALSE)


#2018
url <- paste0(link, "chirps-v2.0.2018.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2018.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.06.tif.gz", remove = FALSE, )

download.file(url, "chirps-v2.0.2018.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2018.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2018.12.tif.gz", remove = FALSE)


#2019
url <- paste0(link, "chirps-v2.0.2019.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2019.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.06.tif.gz", remove = FALSE, )

download.file(url, "chirps-v2.0.2019.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2019.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2019.12.tif.gz", remove = FALSE)

#2020
url <- paste0(link, "chirps-v2.0.2020.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2020.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.06.tif.gz", remove = FALSE, )

download.file(url, "chirps-v2.0.2020.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2020.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2020.12.tif.gz", remove = FALSE)


#2021
url <- paste0(link, "chirps-v2.0.2021.12.tif.gz") #Update to appropriate month and year

download.file(url, "chirps-v2.0.2021.01.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.01.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.02.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.02.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.03.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.03.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.04.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.04.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.05.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.05.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.06.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.06.tif.gz", remove = FALSE, )

download.file(url, "chirps-v2.0.2021.07.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.07.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.08.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.08.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.09.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.09.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.10.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.10.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.11.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.11.tif.gz", remove = FALSE)

download.file(url, "chirps-v2.0.2021.12.tif.gz")
R.utils::gunzip("chirps-v2.0.2021.12.tif.gz", remove = FALSE)



#####  Read in raster data #####
setwd("C:/Users/Colleen/NU-malaria-team Dropbox/data/africa_health_district_climate/climate/africa/chirps_monthly_precip")

chirps_AFR_2011_08 <-  raster("chirps-v2.0.2011.08.tif") 
print(chirps_AFR_2011_08) #longlat WGS84
chirps_AFR_2011_08@data

chirps_AFR_2010_01 <-  raster("chirps-v2.0.2010.01.tif") 
chirps_AFR_2010_02 <-  raster("chirps-v2.0.2010.02.tif") 
chirps_AFR_2010_03 <-  raster("chirps-v2.0.2010.03.tif") 
chirps_AFR_2010_04 <-  raster("chirps-v2.0.2010.04.tif") 
chirps_AFR_2010_05 <-  raster("chirps-v2.0.2010.05.tif") 
chirps_AFR_2010_06 <-  raster("chirps-v2.0.2010.06.tif") 
chirps_AFR_2010_07 <-  raster("chirps-v2.0.2010.07.tif") 
chirps_AFR_2010_08 <-  raster("chirps-v2.0.2010.08.tif") 
chirps_AFR_2010_09 <-  raster("chirps-v2.0.2010.09.tif") 
chirps_AFR_2010_10 <-  raster("chirps-v2.0.2010.10.tif") 
chirps_AFR_2010_11 <-  raster("chirps-v2.0.2010.11.tif") 
chirps_AFR_2010_12 <-  raster("chirps-v2.0.2010.12.tif") 

chirps_AFR_2011_01 <-  raster("chirps-v2.0.2011.01.tif") 
chirps_AFR_2011_02 <-  raster("chirps-v2.0.2011.02.tif") 
chirps_AFR_2011_03 <-  raster("chirps-v2.0.2011.03.tif") 
chirps_AFR_2011_04 <-  raster("chirps-v2.0.2011.04.tif") 
chirps_AFR_2011_05 <-  raster("chirps-v2.0.2011.05.tif") 
chirps_AFR_2011_06 <-  raster("chirps-v2.0.2011.06.tif") 
chirps_AFR_2011_07 <-  raster("chirps-v2.0.2011.07.tif") 
chirps_AFR_2011_08 <-  raster("chirps-v2.0.2011.08.tif") 
chirps_AFR_2011_09 <-  raster("chirps-v2.0.2011.09.tif") 
chirps_AFR_2011_10 <-  raster("chirps-v2.0.2011.10.tif") 
chirps_AFR_2011_11 <-  raster("chirps-v2.0.2011.11.tif") 
chirps_AFR_2011_12 <-  raster("chirps-v2.0.2011.12.tif") 

chirps_AFR_2012_01 <-  raster("chirps-v2.0.2012.01.tif") 
chirps_AFR_2012_02 <-  raster("chirps-v2.0.2012.02.tif") 
chirps_AFR_2012_03 <-  raster("chirps-v2.0.2012.03.tif") 
chirps_AFR_2012_04 <-  raster("chirps-v2.0.2012.04.tif")
chirps_AFR_2012_05 <-  raster("chirps-v2.0.2012.05.tif") 
chirps_AFR_2012_06 <-  raster("chirps-v2.0.2012.06.tif") 
chirps_AFR_2012_07 <-  raster("chirps-v2.0.2012.07.tif") 
chirps_AFR_2012_08 <-  raster("chirps-v2.0.2012.08.tif") 
chirps_AFR_2012_09 <-  raster("chirps-v2.0.2012.09.tif") 
chirps_AFR_2012_10 <-  raster("chirps-v2.0.2012.10.tif")
chirps_AFR_2012_11 <-  raster("chirps-v2.0.2012.11.tif")
chirps_AFR_2012_12 <-  raster("chirps-v2.0.2012.12.tif")


chirps_AFR_2013_01 <-  raster("chirps-v2.0.2013.01.tif") 
chirps_AFR_2013_02 <-  raster("chirps-v2.0.2013.02.tif") 
chirps_AFR_2013_03 <-  raster("chirps-v2.0.2013.03.tif") 
chirps_AFR_2013_04 <-  raster("chirps-v2.0.2013.04.tif")
chirps_AFR_2013_05 <-  raster("chirps-v2.0.2013.05.tif") 
chirps_AFR_2013_06 <-  raster("chirps-v2.0.2013.06.tif") 
chirps_AFR_2013_07 <-  raster("chirps-v2.0.2013.07.tif") 
chirps_AFR_2013_08 <-  raster("chirps-v2.0.2013.08.tif") 
chirps_AFR_2013_09 <-  raster("chirps-v2.0.2013.09.tif") 
chirps_AFR_2013_10 <-  raster("chirps-v2.0.2013.10.tif")
chirps_AFR_2013_11 <-  raster("chirps-v2.0.2013.11.tif")
chirps_AFR_2013_12 <-  raster("chirps-v2.0.2013.12.tif")

chirps_AFR_2014_01 <-  raster("chirps-v2.0.2014.01.tif") 
chirps_AFR_2014_02 <-  raster("chirps-v2.0.2014.02.tif") 
chirps_AFR_2014_03 <-  raster("chirps-v2.0.2014.03.tif") 
chirps_AFR_2014_04 <-  raster("chirps-v2.0.2014.04.tif") 
chirps_AFR_2014_05 <-  raster("chirps-v2.0.2014.05.tif")
chirps_AFR_2014_06 <-  raster("chirps-v2.0.2014.06.tif") 
chirps_AFR_2014_07 <-  raster("chirps-v2.0.2014.07.tif") 
chirps_AFR_2014_08 <-  raster("chirps-v2.0.2014.08.tif") 
chirps_AFR_2014_09 <-  raster("chirps-v2.0.2014.09.tif") 
chirps_AFR_2014_10 <-  raster("chirps-v2.0.2014.10.tif") 
chirps_AFR_2014_11 <-  raster("chirps-v2.0.2014.11.tif") 
chirps_AFR_2014_12 <-  raster("chirps-v2.0.2014.12.tif") 

chirps_AFR_2015_01 <-  raster("chirps-v2.0.2015.01.tif") 
chirps_AFR_2015_02 <-  raster("chirps-v2.0.2015.02.tif") 
chirps_AFR_2015_03 <-  raster("chirps-v2.0.2015.03.tif") 
chirps_AFR_2015_04 <-  raster("chirps-v2.0.2015.04.tif") 
chirps_AFR_2015_05 <-  raster("chirps-v2.0.2015.05.tif")
chirps_AFR_2015_06 <-  raster("chirps-v2.0.2015.06.tif") 
chirps_AFR_2015_07 <-  raster("chirps-v2.0.2015.07.tif") 
chirps_AFR_2015_08 <-  raster("chirps-v2.0.2015.08.tif") 
chirps_AFR_2015_09 <-  raster("chirps-v2.0.2015.09.tif") 
chirps_AFR_2015_10 <-  raster("chirps-v2.0.2015.10.tif") 
chirps_AFR_2015_11 <-  raster("chirps-v2.0.2015.11.tif") 
chirps_AFR_2015_12 <-  raster("chirps-v2.0.2015.12.tif") 

chirps_AFR_2016_01 <-  raster("chirps-v2.0.2016.01.tif") 
chirps_AFR_2016_02 <-  raster("chirps-v2.0.2016.02.tif") 
chirps_AFR_2016_03 <-  raster("chirps-v2.0.2016.03.tif") 
chirps_AFR_2016_04 <-  raster("chirps-v2.0.2016.04.tif") 
chirps_AFR_2016_05 <-  raster("chirps-v2.0.2016.05.tif") 
chirps_AFR_2016_06 <-  raster("chirps-v2.0.2016.06.tif") 
chirps_AFR_2016_07 <-  raster("chirps-v2.0.2016.07.tif") 
chirps_AFR_2016_08 <-  raster("chirps-v2.0.2016.08.tif") 
chirps_AFR_2016_09 <-  raster("chirps-v2.0.2016.09.tif") 
chirps_AFR_2016_10 <-  raster("chirps-v2.0.2016.10.tif") 
chirps_AFR_2016_11 <-  raster("chirps-v2.0.2016.11.tif") 
chirps_AFR_2016_12 <-  raster("chirps-v2.0.2016.12.tif") 

chirps_AFR_2017_01 <-  raster("chirps-v2.0.2017.01.tif") 
chirps_AFR_2017_02 <-  raster("chirps-v2.0.2017.02.tif") 
chirps_AFR_2017_03 <-  raster("chirps-v2.0.2017.03.tif") 
chirps_AFR_2017_04 <-  raster("chirps-v2.0.2017.04.tif") 
chirps_AFR_2017_05 <-  raster("chirps-v2.0.2017.05.tif") 
chirps_AFR_2017_06 <-  raster("chirps-v2.0.2017.06.tif") 
chirps_AFR_2017_07 <-  raster("chirps-v2.0.2017.07.tif") 
chirps_AFR_2017_08 <-  raster("chirps-v2.0.2017.08.tif") 
chirps_AFR_2017_09 <-  raster("chirps-v2.0.2017.09.tif") 
chirps_AFR_2017_10 <-  raster("chirps-v2.0.2017.10.tif") 
chirps_AFR_2017_11 <-  raster("chirps-v2.0.2017.11.tif") 
chirps_AFR_2017_12 <-  raster("chirps-v2.0.2017.12.tif") 

chirps_AFR_2018_01 <-  raster("chirps-v2.0.2018.01.tif") 
chirps_AFR_2018_02 <-  raster("chirps-v2.0.2018.02.tif") 
chirps_AFR_2018_03 <-  raster("chirps-v2.0.2018.03.tif") 
chirps_AFR_2018_04 <-  raster("chirps-v2.0.2018.04.tif") 
chirps_AFR_2018_05 <-  raster("chirps-v2.0.2018.05.tif") 
chirps_AFR_2018_06 <-  raster("chirps-v2.0.2018.06.tif") 
chirps_AFR_2018_07 <-  raster("chirps-v2.0.2018.07.tif") 
chirps_AFR_2018_08 <-  raster("chirps-v2.0.2018.08.tif") 
chirps_AFR_2018_09 <-  raster("chirps-v2.0.2018.09.tif") 
chirps_AFR_2018_10 <-  raster("chirps-v2.0.2018.10.tif") 
chirps_AFR_2018_11 <-  raster("chirps-v2.0.2018.11.tif") 
chirps_AFR_2018_12 <-  raster("chirps-v2.0.2018.12.tif") 

chirps_AFR_2019_01 <-  raster("chirps-v2.0.2019.01.tif") 
chirps_AFR_2019_02 <-  raster("chirps-v2.0.2019.02.tif") 
chirps_AFR_2019_03 <-  raster("chirps-v2.0.2019.03.tif") 
chirps_AFR_2019_04 <-  raster("chirps-v2.0.2019.04.tif") 
chirps_AFR_2019_05 <-  raster("chirps-v2.0.2019.05.tif") 
chirps_AFR_2019_06 <-  raster("chirps-v2.0.2019.06.tif") 
chirps_AFR_2019_07 <-  raster("chirps-v2.0.2019.07.tif") 
chirps_AFR_2019_08 <-  raster("chirps-v2.0.2019.08.tif") 
chirps_AFR_2019_09 <-  raster("chirps-v2.0.2019.09.tif") 
chirps_AFR_2019_10 <-  raster("chirps-v2.0.2019.10.tif") 
chirps_AFR_2019_11 <-  raster("chirps-v2.0.2019.11.tif") 
chirps_AFR_2019_12 <-  raster("chirps-v2.0.2019.12.tif") 

chirps_AFR_2020_01 <-  raster("chirps-v2.0.2020.01.tif") 
chirps_AFR_2020_02 <-  raster("chirps-v2.0.2020.02.tif") 
chirps_AFR_2020_03 <-  raster("chirps-v2.0.2020.03.tif") 
chirps_AFR_2020_04 <-  raster("chirps-v2.0.2020.04.tif") 
chirps_AFR_2020_05 <-  raster("chirps-v2.0.2020.05.tif") 
chirps_AFR_2020_06 <-  raster("chirps-v2.0.2020.06.tif") 
chirps_AFR_2020_07 <-  raster("chirps-v2.0.2020.07.tif") 
chirps_AFR_2020_08 <-  raster("chirps-v2.0.2020.08.tif") 
chirps_AFR_2020_09 <-  raster("chirps-v2.0.2020.09.tif") 
chirps_AFR_2020_10 <-  raster("chirps-v2.0.2020.10.tif") 
chirps_AFR_2020_11 <-  raster("chirps-v2.0.2020.11.tif") 
chirps_AFR_2020_12 <-  raster("chirps-v2.0.2020.12.tif") 

chirps_AFR_2021_01 <-  raster("chirps-v2.0.2021.01.tif") 
chirps_AFR_2021_02 <-  raster("chirps-v2.0.2021.02.tif") 
chirps_AFR_2021_03 <-  raster("chirps-v2.0.2021.03.tif") 
chirps_AFR_2021_04 <-  raster("chirps-v2.0.2021.04.tif") 
chirps_AFR_2021_05 <-  raster("chirps-v2.0.2021.05.tif") 
chirps_AFR_2021_06 <-  raster("chirps-v2.0.2021.06.tif") 
chirps_AFR_2021_07 <-  raster("chirps-v2.0.2021.07.tif") 
chirps_AFR_2021_08 <-  raster("chirps-v2.0.2021.08.tif") 
chirps_AFR_2021_09 <-  raster("chirps-v2.0.2021.09.tif") 
chirps_AFR_2021_10 <-  raster("chirps-v2.0.2021.10.tif") 
chirps_AFR_2021_11 <-  raster("chirps-v2.0.2021.11.tif") 
chirps_AFR_2021_12 <-  raster("chirps-v2.0.2021.12.tif") 


#Assigned NA values
NAvalue(precip_all_months) <- -9999

