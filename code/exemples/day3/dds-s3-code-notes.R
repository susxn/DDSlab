# Operaciones Basicas

## Ejemplo APNIC
apnic <- readRDS("C:/DEVEL/code/whois/inst/tempdata/apnic.rds")

### Inspeccion
summary(apnic)
str(apnic)
names(apnic)

### Seleccion
asn <- apnic$`aut-num`
inet <- apnic$inetnum
summary(asn)
summary(inet)
dim(inet)
attributes(inet)

# Lectura / Escritura

## Descarga de un data set
myurl <- "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda"
myfile <- tempfile(fileext = ".rda")
download.file(url = myurl, destfile = myfile)
load(myfile)

## Ejemplo JSON
cves.src <- tempfile(fileext = ".json")
download.file(url = "https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-recent.json.gz", destfile = cves.src)
raw.cves <- jsonlite::fromJSON(cves.src)
cves.raw <- raw.cves$CVE_Items$cve
conf.raw <- raw.cves$CVE_Items$configurations
impc.raw <- raw.cves$CVE_Items$impact

### exploracion
summary(cves.raw)
names(cves.raw)
class(cves.raw$description)
df.descr <- cves.raw$description
class(cves.raw$description$description_data)
df.descr  <- cves.raw$description$description_data

### transformacion
description <- unlist(lapply(cves.raw$description$description_data, jsonlite::toJSON))

