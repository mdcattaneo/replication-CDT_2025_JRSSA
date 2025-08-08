#******************************************************************************#
#******************************************************************************#
#** README for Replication Files for ******************************************#
#** Randomization inference for before-and-after studies with multiple units **# 
#********** An application to a criminal procedure reform in Uruguay **********#
#* Matías Cattaneo, Carlos Díaz, and Rocío Titiunik */
#******************************************************************************#
#******************************************************************************#

CDT_functions.R					: Functions to perform randomization-based analyses. Called by other scripts.
CDT_Replication_01-Analysis-Tables1to4.R	: Tables 1 through 4 in main paper
CDT_Replication_01-Analysis-TotalReports1618.R	: Figure 1 in main paper
CDT_Replication_01-Analysis-WindowSelection.R	: Window selector in main paper, and in Section SA-2 of Supplemental Appendix 
CDT_Replication_02-SA-DailyChanges.R		: Figure SA-2 in Supplemental Appendix
CDT_Replication_02-SA-EventStudy.do		: Event study analysis in in Section SA-5 of Supplemental Appendix 
CDT_Replication_02-SA-PlaceboDate.R		: Figure SA-8 in Supplemental Appendix
CDT_Replication_02-SA-PlaceboDate1819.R		: Figure SA-8 in Supplemental Appendix
CDT_Replication_02-SA-PlaceboDay.R		: Figure SA-6 in Supplemental Appendix
CDT_Replication_02-SA-ReportingDelay.R		: Figure SA-7 in Supplemental Appendix
CDT_Replication_02-SA-Tables-SA1-SA2-SA3.R	: Tables SA-1, SA-2, and SA-3 in Supplemental Appendix
crime2017_15delay.csv				: Dataset called by CDT_Replication_02-SA-ReportingDelay.R	 
crime2017_30delay.csv				: Dataset called by CDT_Replication_02-SA-ReportingDelay.R	
crime2017_45delay.csv				: Dataset called by CDT_Replication_02-SA-ReportingDelay.R	
Data_daily.dta					: Dataset for analysis (daily level)
Data_daily_barrio.dta				: Dataset for analysis (daily by neighborhood level)
README.txt					: This file