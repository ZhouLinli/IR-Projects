# Institutional Research Projects for Data Reporting
Welcome! This repository holds R codes for institutional research (IR) projects in Data Reporting. You will find R codes for the processes of data extraction, data cleaning of discrepancies, data merging, and data analysis.

The R scripts/markdowns are developed for reporting to the following external agencies and internal reporting purposes:

## Integrated Postsecondary Education Data System (IPEDS)
- The IPEDScomplete.cached.Rmd file include R codes for merging historical graduate data from databses. The merged dataset can be used for one year or multiple year's graduated students' demography, major, and program information. 

## National Student Clearinghouse (NSC)
- The NationalStudentClearinghosue.Rmd include R codes for creating list of students to be uploaded on the NSC website for using the StudentTracker tool. The file made strictly followed the [NSC guidelines for uploading data files](https://www.studentclearinghouse.org/blog/ufaqs/what-is-the-file-layout-for-the-studenttracker-request-file/).

## Princeton Review
- The CourseRerpoting.R and EnrollDatamartReporting files include codes to extract enrollment data anc ourse information from the unviersity database. The files are made for reporting to [the Princeton Review Entreprenuership Program Ranking](https://www.princetonreview.com/college-rankings/top-entrepreneur).

## Internal reporting
- The AggregateData.Viz.md file include both R codes and visualization output of a project using course enrollment data. The project compares different [placement methods' impact](https://ss.marin.edu/assessment/placement-english) on student enrollment in English courses of different levels. Findings aroudn the differential effect across racial groups are also presented.
- The datafreeze.Rmd file include R codes for identifying data discrepancies between mutiple internal databases ([datamart](https://www.oracle.com/autonomous-database/what-is-data-mart/#:~:text=A%20data%20mart%20is%20a%20simple%20form%20of%20a%20data,fewer%20sources%20than%20data%20warehouses.) and Registrar's database). Results from cross-databases checking lead to updates and corrections of codes in the database systems.
