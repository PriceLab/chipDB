#----------------------------------------------------------------------------------------------------
#' @import methods
#' @import RPostgreSQL
#' @importFrom AnnotationDbi select
#' @import org.Hs.eg.db
#'
#' @title chipDB-class
#'
#' @name chipDB-class
#' @rdname chipDB-class
#' @aliases chipDB
#' @exportClass chipDB
#'

.chipDB <- setClass("chipDB",
                    representation = representation(
                       genomeName="character",
                       db.chipAtlas="PostgreSQLConnection",
                       db.remap="PostgreSQLConnection")
                    )

#----------------------------------------------------------------------------------------------------------
setGeneric('getGenomeName', signature='obj', function(obj) standardGeneric('getGenomeName'))
setGeneric('getHits', signature='obj', function(obj, chrom, start, end, TF=NA_character_) standardGeneric('getHits'))
#----------------------------------------------------------------------------------------------------------
#' Define an object of class chipDB
#'
#' @description
#' Unified access to local ISB ChIP-seq databases
#'
#' @rdname chipDB-class
#'
#' @param quiet A logical object, default TRUE
#'
#' @return An object of the chipDB class
#' @export
#'
chipDB <- function(quiet=TRUE)
{
    db.chipAtlas <- dbConnect(PostgreSQL(), user= "trena", password="trena", dbname="chipatlas", host="khaleesi")
    db.remap <- dbConnect(PostgreSQL(), user= "trena", password="trena", dbname="hg38", host="khaleesi")

    obj <- .chipDB(genomeName="hg38",
                   db.chipAtlas=db.chipAtlas,
                   db.remap=db.remap)

} # chipDB, the constructor
#----------------------------------------------------------------------------------------------------
#' get the name of the genome currently in use: TODO mapping to hg19 could be added
#'
#' @rdname getGenomeName
#' @aliases getGenomeName
#'
#' @param obj An object of class chipDB
#'
#' @export
#'
setMethod('getGenomeName', 'chipDB',

    function(obj){
       obj@genomeName
       })

#------------------------------------------------------------------------------------------------------------------------
#' get the ChiP-seq hits in the specified region, optionally limited to a specified TF
#'
#' @rdname getHits
#' @aliases getHits
#'
#' @param obj An object of class chipDB
#' @param chrom A character object
#' @param start numeric
#' @param end numeric
#' @param TF A character object
#'
#' @export
#'
setMethod('getHits', 'chipDB',

    function(obj, chrom, start, end, TF=NA_character_){
       #browser()
       #xyz <- 99

       query.1 <- sprintf("select * from peaks where chrom='%s' and start >= %d and endpos <= %d limit 3", chrom, start, end)
       tbl.1 <- dbGetQuery(obj@db.chipAtlas, query.1)

       query.2 <- sprintf("select * from chipseq where chrom='%s' and start >= %d and endpos <= %d", chrom, start, end)
       tbl.2 <- dbGetQuery(obj@db.remap, query.2)

       tbl.2
       })

#------------------------------------------------------------------------------------------------------------------------
