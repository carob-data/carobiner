

.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(1500, getOption("timeout")))
	check_package_versions(c("yuri", "vocal"))
}
