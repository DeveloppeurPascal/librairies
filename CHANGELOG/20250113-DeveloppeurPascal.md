# 20250113 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* updated sponsoring links in the GitHub settings and fr/en docs
* added Int64 version of date&time functions in Olf.RTL.DateAndTime
* added BeginUpdate/EndUpdate on TParams and TParamsFile to Save all changes in one time and not for each parameter depending on how you use it
* clear by default the current settings in memory when deleting the param file in TParams and TParmasFile
* added TMonitor protection in TParamsFile methods when manipulating the settings JSONObject (should be enough to avoid thread concurrency problems)
