

.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(1500, getOption("timeout")))
	check_package_versions(c("yuri", "vocal"))
	# Replace yuri's default auth advice (which points to yuri::authenticate)
	# with carob's: credentials live in a 'passwords.R' file in the carob root.
	if (requireNamespace("yuri", quietly = TRUE) &&
		exists("set_auth_advice", envir = asNamespace("yuri"), inherits = FALSE)) {
		try(yuri::set_auth_advice("Set credentials in 'passwords.R' in the carob root directory."), silent = TRUE)
	}
}
