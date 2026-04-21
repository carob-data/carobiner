.carobiner_default_dataverse_guestbook <- function(path = NULL) {
	cur <- getOption("yuri.dataverse.guestbook")
	if (!is.list(cur)) {
		cur <- list()
	}
	def <- list(
		name = "carob",
		email = "carob.data@gmail.com",
		institution = "International Institute of Tropical Agriculture"
	)
	is_empty <- function(x) {
		is.null(x) || (length(x) == 1L && !nzchar(as.character(x)))
	}
	for (nm in names(def)) {
		if (!nm %in% names(cur) || is_empty(cur[[nm]])) {
			cur[[nm]] <- def[[nm]]
		}
	}
	if (!is.null(path) && nzchar(path)) {
		fpwd <- file.path(path, "passwords.R")
		if (file.exists(fpwd)) {
			h <- tryCatch(usr_pwd(path, "HARVARD"), error = function(e) NULL)
			if (is.list(h) && length(h) > 0) {
				if (!is.null(h$username) && nzchar(as.character(h$username)[1L])) {
					cur$name <- as.character(h$username)[1L]
				}
				if (!is.null(h$email) && nzchar(as.character(h$email)[1L])) {
					cur$email <- as.character(h$email)[1L]
				}
				ist <- h$institute
				if (is.null(ist)) ist <- h$institution
				if (!is.null(ist) && nzchar(as.character(ist)[1L])) {
					cur$institution <- as.character(ist)[1L]
				}
				pw <- h$password
				if (!is.null(pw) && length(pw) > 0 && nzchar(as.character(pw)[1L])) {
					Sys.setenv(DATAVERSE_API_TOKEN = as.character(pw)[1L])
				}
			}
		}
	}
	options(yuri.dataverse.guestbook = cur)
	invisible(TRUE)
}


.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(1500, getOption("timeout")))
	.carobiner_default_dataverse_guestbook(NULL)
}


