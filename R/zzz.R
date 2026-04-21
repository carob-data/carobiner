.carobiner_default_dataverse_guestbook <- function() {
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
	options(yuri.dataverse.guestbook = cur)
	invisible(TRUE)
}


.onLoad <- function(libname, pkgname) {
	httr::timeout(15)
	options(timeout = max(1500, getOption("timeout")))
	.carobiner_default_dataverse_guestbook()
}


