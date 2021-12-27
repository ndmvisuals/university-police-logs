# university-police-logs

The University of Maryland Police Department publishes daily crime and incident logs which provide basic information on all calls for 
service the police department responds to, and arrest report ledgers which provides basic information on arrests and citations.

Capital News Service built a web scraper to download and aggregate this data daily and display it on this dashboard. 
See the dashboard - that displays the data interactively- [here](https://cnsmaryland.org/interactives/fall2021/umd-crime/index.html).

If a service call results in an arrest or citation, then the UMPD case number will appear in the arrest ledger. 
In order to classify the primary type of the arrest/citation, the crime and incident log is joined to the arrest ledger 
by UMPD case number. However, if the case number is not included in the crime and incident log, its type is classified as N/A. 
A UMPD case number that appears in the arrest ledger could have multiple charges that differ from the single classification in the 
crime and incident log.
