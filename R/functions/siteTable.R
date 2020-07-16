
siteTable <- function(){
  tribble(
    ~Site, ~`UK Region`, ~`Site description`, ~`Oak decline types present`, ~`Survey date`,
    1, 'West Midlands', 'Oak dominated amenity woodland, parkland plantation.', 'AOD/COD', 'June 2017',
    2, 'East of England', 'Oak dominated woodland managed for timber.', 'AOD/COD', 'September 2016',
    3, 'South West', 'Oak dominated plantation managed for timber.', 'COD', 'August 2016',
    4, 'East of England', 'Oak dominated woodland managed for timber, with lapsed lime coppice.', 'AOD/COD', 'June 2017',
    5, 'South East', 'Parkland with shelterbelts.', 'AOD/COD', 'July 2017',
    6, 'South West', 'Open high forest, mown grass below.', 'AOD/COD', 'June 2017',
    7, 'London', 'Oak dominated urban woodland, boundary belt of park.', 'AOD/COD', 'August 2017',
    8, 'South West', 'Oak dominated amenity woodland.', 'COD', 'August 2016',
    9, 'East of England', 'Oak dominated woodland managed for timber.', 'AOD/COD', 'June 2017',
  )
}