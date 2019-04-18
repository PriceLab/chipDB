library(RUnit)
library(chipDB)
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
if(!exists("cdb"))
   cdb <- chipDB(quiet=FALSE)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_getHits()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor = function ()
{
   printf("--- test_constructor")
   checkTrue(is(cdb) %in% "chipDB")

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_getHits <- function()
{
   printf("--- test_getHits")
     # small region on chr3 with hits in both databases
   tbl <- getHits(cdb, chrom="chr3", start=48200000, end=48210700)
   checkTrue(nrow(tbl) >  100)
   checkTrue(all(c("chipAtlas", "remap") %in% tbl$database))
   checkEquals(colnames(tbl), c("chrom", "start", "end", "tf", "tissueOrCellType", "score", "database"))

} # test_getHits
#------------------------------------------------------------------------------------------------------------------------
