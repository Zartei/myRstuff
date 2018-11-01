print("Hello World");

if (!exists('settings')) { settings <- as.environment(list()); };
if (is.environment(settings)) { settings$dbConnection = "Driver={SQL Server};Server=(local);Database=T618;Trusted_Connection=yes"; }
library("RODBC", lib.loc = "C:/Program Files/Microsoft/R Client/R_SERVER/library");

channel <- odbcDriverConnect(settings$dbConnection);
CurrentTables <- sqlQuery(channel, 'EXECUTE dbo.getTableNames', stringsAsFactors = FALSE);

## sqlTables(channel,schema = "dbo");
InputDataSet <- sqlQuery(channel, "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = 'dbo'", stringsAsFactors = FALSE);

odbcClose(channel);

help("sqlQuery");

for (x in CurrentTables[, 3]) {
    curr <- sprintf("EXECUTE dbo.GetValue %s", x);
    assign(
        paste("", x, sep = ""),
        sqlQuery(channel, curr, stringsAsFactors = FALSE)
    );
}



outside <- function(vector) {
    vl <- c(vector[1], vector[length(vector)])
    return(vl);
    }
;
dry_principle <- c("Don't", "repeat", "yourself", "or", "others")

outside(dry_principle)

splitTrainSet <- function(DF = 0) {
    x <- data.frame("SN" = 1:2, "Age" = c(21, 15), "Name" = c("John", "Dora"))
    y <- data.frame("SN" = 1:2, "Age" = c(21, 15), "Name" = c("John", "Dora"))
    return(list("train" = x, "test" = y));
}

x <- splitTrainSet()

