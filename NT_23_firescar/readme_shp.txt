This shapefile is derived from MODIS images by the Darwin Centre for Bushfire Research, Charles Darwin University. It is provided in the projection GCS_GDA_1994.

Details of the process for mapping fire scars is available on the NAFI website. More detailed information on the mapping process and data reliability can be obtained from the Darwin Centre for Bushfire Research (see email contact below).

This shapefile includes all firescars identified in the current year to date across the Australian rangelands within the geographic limts described in the attached metadata txt file.

The shapefile polygons have three attributes:

ID:   an indentifier for the ploygon
GRIDCODE: a Fire Scar Identifier between 1 and 12 that identifies the month of the mapping update.
MONTH: the month number (January = 1, February= 2 etc.) indicating the month in which the fire scar was detected as being burnt.The accuracy of the dating is limited by the mapping interval.

The shapefile is provided with a .csv file with more details on the fire scar data linked to the GRIDCODE attribute:

FSID: 		The identifier for the month of each mapping event. This matches the attribute "GRIDCODE" in the shapefile.
Mapping_period: the end date of the period mapped
Month: 		the month number (January = 1, February= 2 etc.) indicating the approximate time the fire scar was detected as being burnt
Region: 	the region mapped (NT, QLDS, QLDN, WA)
Satellite: 	the MODIS satellite that the image came from (usually terra)
Upload_date: 	the date the fire scars were uploaded to the NAFI site
File_name: 	the file_name of the upload (for admin/tracking purposes)
Current: 	indicates the most up to date mapping for each region (will the final mapping periods of the year)
Comment: 	provides details on the fire scar mapping for that region and that mapping period
RGB values: 	the RGB values allocated to the image data for that fire scar when displayed on the NAFI site


Contacts:
General enquiries: nafi@cdu.edu.au
MODIS information: www.ga.gov.au